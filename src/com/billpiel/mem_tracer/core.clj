(ns com.billpiel.mem-tracer.core)

(def ^{:doc "Current stack depth of traced function calls." :private true :dynamic true}
  *trace-depth* 0)

(def ^:dynamic *trace-log-parent* nil)

(defn ^{:private true} start-trace
  "This function is called by trace. Prints to standard output, but
may be rebound to do anything you like. 'name' is optional."
  [trace-log entry]
  (swap! trace-log
         conj
         entry))  ;; string!!

(defn ^{:private true} end-trace
  "This function is called by trace. Prints to standard output, but
may be rebound to do anything you like. 'name' is optional."
  [trace-log idx entry]
  (swap! trace-log
         update-in
         [idx]
         #(merge % entry)))

(defn ^{:private true} trace-indent
  "Returns an indentation string based on *trace-depth*"
  []
  (apply str (take *trace-depth* (repeat "| "))))

(defn ^{:skip-wiki true} trace-fn-call
  "Traces a single call to a function f with args. 'name' is the
symbol name of the function."
  [root name f args]
  (let [parent (or #spy/d *trace-log-parent*
                   #spy/d root)
        _ #spy/d parent
        this ^::entry {:id #spy/d (str (gensym ""))
                       :parent-id (:id parent)
                       :depth (-> parent :depth inc)
                       :name name
                       :args (vec args)
                       :children (atom [])
                       :started-at (java.util.Date.)}
        idx (-> (start-trace (:children parent)
                             this)
                count
                dec)]
    (let [value (binding [*trace-log-parent* this]
                  (apply f args))]
      (end-trace (:children parent)
                 idx
                 {:return value
                  :ended-at (java.util.Date.)})
      value)))

(defn ^{:skip-wiki true} trace-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a tracing call;
  otherwise nothing happens. Can be undone with untrace-var.

  In the unary case, v should be a Var object or a symbol to be
  resolved in the current namespace.

  In the binary case, ns should be a namespace object or a symbol
  naming a namespace and s a symbol to be resolved in that namespace."
  ([ns s root]
   (trace-var* (ns-resolve ns s) root))
  ([v root]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)]
     (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::traced not))
       (let [f @v
             vname (str ns "/" s)]
         (doto v
           (alter-var-root #(fn tracing-wrapper [& args]
                              (println "IN TRACING WRAPPER!")
                              (trace-fn-call root vname % args)))
           (alter-meta! assoc ::traced f)))))))

(defn ^{:skip-wiki true} untrace-var*
  "Reverses the effect of trace-var / trace-vars / trace-ns for the
  given Var, replacing the traced function with the original, untraced
  version. No-op for non-traced Vars.

  Argument types are the same as those for trace-var."
  ([ns s]
     (untrace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)
           f  ((meta v) ::traced)]
       (when f
         (doto v
           (alter-var-root (constantly ((meta v) ::traced)))
           (alter-meta! dissoc ::traced))))))

#_ (defmacro trace-vars
     "Trace each of the specified Vars.
  The arguments may be Var objects or symbols to be resolved in the
  current namespace."
     [& vs]
     `(do ~@(for [x vs] `(trace-var* (quote ~x)))))

(defn ^{:skip-wiki true} trace-ns*
  "Replaces each function from the given namespace with a version wrapped
  in a tracing call. Can be undone with untrace-ns. ns should be a namespace
  object or a symbol.

  No-op for clojure.core and clojure.tools.trace."
  [ns root]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core com.billpiel.mem-tracer.core} (.name ns))
      (let [ns-fns (->> ns ns-interns vals (filter (comp fn? var-get)))]
        (doseq [f ns-fns]
          (trace-var* f root))))))

(defn default-root
  [& {:as m}]
  (merge ^::entry {:id (name (gensym "root"))
                   :depth 0
                   :children (atom [])}
         m))

(defmacro trace-ns
  "Trace all fns in the given name space."
  [ns & [root]]
  `(let [root# (or ~root (default-root))]
     (trace-ns* ~ns root#)
     root#))

(defn ^{:skip-wiki true} untrace-ns*
  "Reverses the effect of trace-var / trace-vars / trace-ns for the
  Vars in the given namespace, replacing each traced function from the
  given namespace with the original, untraced version."
  [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
          (untrace-var* f))))

(defmacro untrace-ns
  "Untrace all fns in the given name space."
  [ns]
  `(untrace-ns* ~ns))

(defn deref-children
  [v]
  (clojure.walk/prewalk #(if (-> %
                                 meta
                                 ::entry)
                           (update-in % [:children] deref)
                           %)
                        v))
