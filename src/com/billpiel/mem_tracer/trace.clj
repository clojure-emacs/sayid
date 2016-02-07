(ns com.billpiel.mem-tracer.trace
  (require [com.billpiel.mem-tracer.util.other :as util]))

(def ^:dynamic *trace-log-parent* nil)

(defn now [] (System/currentTimeMillis))

(defn mk-tree
  [& {:keys [id-prefix parent]}]
  (let [id (-> id-prefix
               gensym
               keyword)
        path (conj (or (:path parent)
                       [])
                   id)]
    ^::tree {:id id
             :path path
             :depth (or (some-> parent :depth inc) 0)
             :children (atom [])}))

(defn mk-fn-tree
  [& {:keys [parent name args meta]}]
  (merge (mk-tree :parent parent)
         {:name name
          :args (vec args)
          :meta meta
          :arg-map  (delay (util/arg-match (-> meta
                                               :arglists
                                               vec)
                                           args))
          :started-at (now)}))

(defn StackTraceElement->map
  [^StackTraceElement o]
  {:class-name (.getClassName o)
   :file-name (.getFileName o)
   :method-name (.getMethodName o)
   :line-number (.getLineNumber o)})

(defn Throwable->map
  "Constructs a data representation for a Throwable."
  {:added "1.7"}
  [^Throwable o]
  (let [base (fn [^Throwable t]
               (let [m {:type (class t)
                        :message (.getLocalizedMessage t)
                        :at (StackTraceElement->map (get (.getStackTrace t) 0))}
                     data (ex-data t)]
                 (if data
                   (assoc m :data data)
                   m)))
        via (loop [via [], ^Throwable t o]
              (if t
                (recur (conj via t) (.getCause t))
                via))
        ^Throwable root (peek via)
        m {:cause (.getLocalizedMessage root)
           :via (vec (map base via))
           :trace (mapv StackTraceElement->map (.getStackTrace ^Throwable (or root o)))}
        data (ex-data root)]
    (if data
      (assoc m :data data)
      m)))


(defn ^{:private true} start-trace
  "This function is called by trace. Prints to standard output, but
may be rebound to do anything you like. 'name' is optional."
  [trace-log tree]
  (swap! trace-log
         conj
         tree))  ;; string!!

(defn ^{:private true} end-trace
  "This function is called by trace. Prints to standard output, but
may be rebound to do anything you like. 'name' is optional."
  [trace-log idx tree]
  (swap! trace-log
         update-in
         [idx]
         #(merge % tree)))

(defn ^{:skip-wiki true} trace-fn-call
  "Traces a single call to a function f with args. 'name' is the
symbol name of the function."
  [workspace name f args meta']
  (let [parent (or *trace-log-parent*
                   workspace)
        this (mk-fn-tree :parent parent
                          :name name
                          :args args
                          :meta meta')
        idx (-> (start-trace (:children parent)
                             this)
                count
                dec)]
    (let [value (binding [*trace-log-parent* this]
                  (try
                    (apply f args)
                    (catch Throwable t
                      (end-trace (:children parent)
                                 idx
                                 {:throw (Throwable->map t)
                                  :ended-at (now)})
                      (throw t))))]
      (end-trace (:children parent)
                 idx
                 {:return value
                  :ended-at (now)})
      value)))

(defn apply-trace-to-var
  [^clojure.lang.Var v workspace]
  (let [ns (.ns v)
        s  (.sym v)
        m (meta v)
        f @v
        vname (str ns "/" s)]
    (doto v
      (alter-var-root #(fn tracing-wrapper [& args]
                         (trace-fn-call workspace vname % args m)))
      (alter-meta! assoc ::traced [(:id workspace) f]))))

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
           [_ f]  ((meta v) ::traced)]
       (when f
         (doto v
           (alter-var-root (constantly f))
           (alter-meta! dissoc ::traced))))))

(defn ^{:skip-wiki true} trace-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a tracing call;
  otherwise nothing happens. Can be undone with untrace-var.

  In the unary case, v should be a Var object or a symbol to be
  resolved in the current namespace.

  In the binary case, ns should be a namespace object or a symbol
  naming a namespace and s a symbol to be resolved in that namespace."
  ([ns s workspace]
   (trace-var* (ns-resolve ns s) workspace))
  ([v workspace]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
     (when (and (ifn? @v) (-> v meta :macro not))
       (if-let [[traced-id traced-f] (-> v meta ::traced)]
         (when (not= traced-id (:id workspace))
           (untrace-var* v)
           (apply-trace-to-var v workspace))
         (apply-trace-to-var v workspace))))))

(defn ^{:skip-wiki true} trace-ns*
  "Replaces each function from the given namespace with a version wrapped
  in a tracing call. Can be undone with untrace-ns. ns should be a namespace
  object or a symbol.

  No-op for clojure.core and clojure.tools.trace."
  [ns workspace]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core com.billpiel.mem-tracer.core} (.name ns))
      (let [ns-fns (->> ns ns-interns vals (filter (comp fn? var-get)))]
        (doseq [f ns-fns]
          (trace-var* f workspace))))))

(defn ^{:skip-wiki true} untrace-ns*
  "Reverses the effect of trace-var / trace-vars / trace-ns for the
  Vars in the given namespace, replacing each traced function from the
  given namespace with the original, untraced version."
  [ns*]
  (let [ns' (the-ns ns*)
        ns-fns (->> ns' ns-interns vals)]
    (doseq [f ns-fns]
      (untrace-var* f))))

(defmulti trace* (fn [type sym workspace] type))

(defmethod trace* :ns
  [_ sym workspace]
  (trace-ns* sym workspace))

(defmethod trace* :fn
  [_ sym workspace]
  (throw (Exception. "not implemented")))


(defmulti untrace* (fn [type sym] type))

(defmethod untrace* :ns
  [_ sym]
  (untrace-ns* sym))

(defmethod untrace* :fn
  [_ sym]
  (throw (Exception. "not implemented")))
