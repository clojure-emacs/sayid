(ns com.billpiel.mem-tracer.util.other)

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
  (let [arities (map #(list % '(com.billpiel.mem-tracer.util.other/get-env) ) ;; NOTE!  NS/get-env must match this ns
                     arglists)
        args-v (vec args)
        fn1 `(fn ~@arities)]
    (apply-to-map-vals #(if (seq? %) ;; Resolve LazySeqs
                          (apply list %)
                          %)
                       (apply (eval fn1)
                              args-v))))

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
