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
