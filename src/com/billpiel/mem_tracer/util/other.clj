(ns com.billpiel.mem-tracer.util.other)

(defn def-ns-var
  [ws-ns-sym sym v]
  (binding [*ns* (create-ns ws-ns-sym)]
    (eval `(def ~sym '~v))))

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
