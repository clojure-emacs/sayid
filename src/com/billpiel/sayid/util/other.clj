(ns com.billpiel.sayid.util.other
  "General-purpose helpers that don't have a more specific home.  Symbol and
  namespace helpers live in `util.sym`, source-reading in `util.source`."
  (:require [clojure.walk :as walk]
            [com.billpiel.sayid.util.sym :as sym]))

(defn ->int
  [v]
  (try (cond (integer? v) v
             (string? v) (Integer/parseInt v)
             (float? v) (int v))
       (catch Exception e
         nil)))

(defn ->vec
  [v]
  (cond (vector? v) v
        (sequential? v) (vec v)
        (coll? v) (vec v)
        :else [v]))

(defn ->symbol
  [v]
  (cond (= (type v) clojure.lang.Namespace) (symbol (.getName ^clojure.lang.Namespace v))
        (keyword? v) (-> v name symbol)
        (string? v) (symbol v)
        :else (-> v str symbol)))

(defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [`'~k k])))

(defn apply-to-map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)])
                m)))

(defn clear-meta
  [v]
  (with-meta v nil))

(defn cleanse-arglist
  "Clear out pre-condtions and tags."
  [arglist]
  (->> arglist
       clear-meta
       (mapv clear-meta)))

(defn arg-matcher-fn
  [arglists]
  (when (not-empty arglists)
    (let [arities (map #(list % '(com.billpiel.sayid.util.other/get-env)) ;; NOTE!  NS/get-env must match this ns
                       arglists)
          fn1  `(fn ~@arities)]
      (eval fn1))))

(def arg-matcher-fn-memo (memoize arg-matcher-fn))

(defn arg-match
  [arglists args]
  (if (not-empty arglists)
    (let [args-v (vec args)
          matcher-fn (->> arglists
                          (map cleanse-arglist)
                          arg-matcher-fn-memo)]
      (apply matcher-fn args-v))
    (zipmap (range) args)))

(defn arg-match-safe
  [arglists args]
  (try
    (arg-match arglists args)
    (catch Exception e
      nil)))

(defn atom?
  [v]
  (instance? clojure.lang.Atom v))

(defn atom?->
  [maybe-atom]
  (if (atom? maybe-atom)
    @maybe-atom
    maybe-atom))

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
                  (and t (not f)) (constantly obj)
                  (and t-fn (not f)) (constantly obj)
                  :else (constantly f))]
        (fn' obj)))))

(defn just-get-whatever-you-can
  [ns-sym clue]
  (-> clue
      (obj-pred-action-else keyword? :t-fn name)
      (obj-pred-action-else string? :t-fn symbol)
      (obj-pred-action-else symbol? :t-fn #(ns-resolve ns-sym %))
      derefable?->))

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

(defn defalias-macro*
  [alias source]
  (let [body-sym (gensym "body")
        qualified-source (sym/qualify-sym *ns* source)
        source-meta (->> source
                         (ns-resolve *ns*)
                         meta)
        arglists (:arglists source-meta)
        split-at-& (fn [arg-vec]
                     (let [[pre amp post]
                           (partition-by #{'&} arg-vec)]
                       (if (-> arg-vec
                               first
                               #{'&})
                         (concat amp)
                         (concat [(vec pre)] post))))
        forms (map (fn [args]
                     `(~args
                       (clojure.core/seq
                        (clojure.core/concat
                         (clojure.core/list '~qualified-source)
                         ~@(split-at-& args)))))
                   arglists)]
    `(do (defmacro ~alias ~@forms)
         (alter-meta! #'~alias merge
                      (-> (var ~qualified-source)
                          meta
                          (select-keys [:arglists
                                        :doc])
                          (update-in [:doc] #(format "An alias for `%s`.\n%s"
                                                     (name '~source)
                                                     %))))
         #'~alias)))

(defmacro defalias-macro
  [alias source]
  (defalias-macro* alias source))

(defmacro src-in-meta
  [& body]
  `(alter-meta! ~body assoc :source '~body))

(defn back-into
  "Puts the contents of `noob` into a collection of the same type as `orig`."
  [orig noob]
  ((if (seq? orig)
     reverse
     identity)
   (into (or (empty orig)
             [])
         noob)))

(defn deep-zipmap
       [a b]
       (zipmap (tree-seq coll? seq a)
               (tree-seq coll? seq b)))

(defn flatten-map-kv-pairs
  [m]
  (mapcat (fn [[k v]]
            (mapv vector
                  (repeat k)
                  v))
          m))

(defmacro assoc-var-meta-to-fn
  [fn-sym meta-key]
  `(vary-meta ~fn-sym
              assoc
              ~meta-key
              (-> #'~fn-sym
                  meta
                  ~meta-key)))


(defn get-some*
  [f v]
  (cond
    (fn? f)
    (f v)

    (set? f)
    (f v)

    :default
    (get v f)))

(defn get-some
  [coll v]
  (loop [coll coll
         v v]
    (if ((some-fn empty? nil?) coll)
      v
      (let [[f & r] coll]
        (when-let [v' (get-some* f v)]
          (recur r v'))))))


(defn quote-if-sym
  [v]
  (if (symbol? v)
    `'~v
    v))

(defn fn*?
  [maybe]
  (or (fn? maybe)
      (= (type maybe) clojure.lang.MultiFn)))
