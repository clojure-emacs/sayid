(ns com.billpiel.mem-tracer.util.other
  (require [clj-time.core :as time]
           [clj-time.coerce :as time-c]
           [clojure.walk :as walk]))

(defn def-ns-var
  [ws-ns-sym sym v]
  (binding [*ns* (create-ns ws-ns-sym)]
    (eval `(def ~sym '~v))))

(defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [(name k) k])))

(defn apply-to-map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)])
                m)))

(defn arg-match
  [arglists args]
  (if (not-empty arglists)
    (let [arities (map #(list % '(com.billpiel.mem-tracer.util.other/get-env) ) ;; NOTE!  NS/get-env must match this ns
                       arglists)
          args-v (vec args)
          fn1  `(fn ~@arities)]
      (apply-to-map-vals #(if (seq? %) ;; Resolve LazySeqs
                            (apply list %)
                            %)
                         (apply (eval fn1)
                                args-v)))
    (zipmap (range) args)))

(defn qualify-sym
  [ns sym]
  (symbol (name ns)
          (name sym)))

(defn atom?
  [v]
  (instance? clojure.lang.Atom v))

(defn atom?->
  [maybe-atom]
  (if (atom? maybe-atom)
    @maybe-atom
    maybe-atom))

(defn atom?-fn
  [maybe-atom]
  (if (atom? maybe-atom)
    [@maybe-atom
     (fn [newval]
       (reset! maybe-atom newval)
       maybe-atom)
     (fn [f]
       (swap! maybe-atom f)
       maybe-atom)]
    [maybe-atom identity identity]))


(defn derefable?
  [v]
  (instance? clojure.lang.IDeref v))

(defn derefable?->
  [maybe-ideref]
  (if (derefable? maybe-ideref)
    @maybe-ideref
    maybe-ideref))

(defn obj-pred-action-else
  [obj pred & {:keys [t t-fn f f-fn]}]
  (let [pred' (or pred identity)]
    (if (pred' obj)
      (let [fn' (if t-fn
                  t-fn
                  (constantly (if (nil? t)
                                obj
                                t)))]
        (fn' obj))
      (let [fn' (cond
                  f-fn f-fn
                  t (constantly obj)
                  t-fn (constantly obj)
                  :else (constantly f))]
        (fn' obj)))))

(defn just-get-whatever-you-can
  [ns-sym clue]
  (-> clue
      (obj-pred-action-else keyword? :t-fn name)
      (obj-pred-action-else string? :t-fn symbol)
      (obj-pred-action-else symbol? :t-fn #(ns-resolve ns-sym %))
      derefable?->))

(defn diff-dates-in-msec
  [a b]
  (- (-> a time-c/from-date time-c/to-long)
     (-> b time-c/from-date time-c/to-long)))

(defn replace$
  [form]
  (let [$sym `$#
        form' (walk/prewalk-replace {'$ $sym}
                                    form)]
    (if (= form form')
      form
      `((fn [~$sym] ~form')))))

(defmacro $-
  [m & body]
  `(~m ~@(map replace$ body)))

(defmacro defalias
  [alias source]
  `(do (def ~alias ~source)
       (alter-meta! #'~alias merge
                    (-> #'~source
                        meta
                        (select-keys [:arglists
                                      :doc])
                        (update-in [:doc] #(format "An alias for `%s`.\n%s"
                                                   (name '~source)
                                                   %))))))
(defmacro defalias-macro
  [alias source]
  `(do (defmacro ~alias [& body#] `(~'~source '~@body#))
       (alter-meta! #'~alias merge
                    (-> #'~source
                        meta
                        (select-keys [:arglists
                                      :doc])
                        (update-in [:doc] #(format "An alias for `%s`.\n%s"
                                                   (name '~source)
                                                   %))))
       #'~alias))

(defn source-fn-var
  [fn-var]
  (->> fn-var
       meta ((juxt :ns
                   (constantly "/")
                   :name))
       (apply str)
       symbol
       clojure.repl/source-fn))

(defn hunt-down-source
  [fn-sym]
  (let [{:keys [source file line]} (-> fn-sym
                                       resolve
                                       meta)]
    (or source
        (read-string (or
                      (clojure.repl/source-fn fn-sym)
                      (->> file
                           slurp
                           clojure.string/split-lines
                           (drop (- line 1))
                           clojure.string/join)
                      "nil")))))

(defmacro src-in-meta
  [& body]
  `(alter-meta! ~body assoc :source '~body))

(src-in-meta
 defn fff
 [x] (-> x
         inc
         inc))

(-> fff var meta :source clojure.walk/macroexpand-all)
(eval '(do (declare x) (let [x 4] (eval '(inc x)))))

#_ (do
     (defn ff [] 1)

     (->> #'ff
          meta ((juxt :ns
                      (constantly "/")
                      :name))
          (apply str)
          symbol
          clojure.repl/source-fn)

     (defn f1
       [a b c]
       (inc (let [d 4
                  e 5]
              (-> a
                  inc
                  (+ d)))))

     (f1 1 2 3)

     (clojure.pprint/pprint (clojure.walk/macroexpand-all '(defn f1
                                                             [a b c]
                                                             (inc (let [d 4
                                                                        e 5]
                                                                    (-> a
                                                                        inc
                                                                        (+ d)))))))
     (def f1
       (fn* ([a b c]
             (inc (let* [d 4
                         e 5]
                        (+
                         (inc a)
                         d))))))


     (comment))
