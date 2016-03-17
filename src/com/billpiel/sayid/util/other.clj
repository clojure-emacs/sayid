(ns com.billpiel.sayid.util.other
  (require [clj-time.core :as time]
           [clj-time.coerce :as time-c]
           [clojure.walk :as walk]))

(defn def-ns-var
  [ws-ns-sym sym v]
  (binding [*ns* (create-ns ws-ns-sym)]
    (eval `(def ~sym '~v))))

(defn eval-in-ns
  [ns-sym form]
  (binding [*ns* (create-ns ns-sym)]
    (eval form)))

(defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [`'~k k])))

(defn apply-to-map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)])
                m)))

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
          matcher-fn (arg-matcher-fn-memo arglists)]
      (apply-to-map-vals #(if (seq? %) ;; Resolve LazySeqs
                            (apply list %)
                            %)
                         (apply matcher-fn
                                args-v)))
    (zipmap (range) args)))

(defn qualify-sym
  [ns sym]
  (symbol (str ns)
          (str sym)))

(defmacro fully-qualify-sym
  [sym]
  `(let [m# (-> ~sym
                resolve
                meta)
         ns# (-> m# :ns str)
         name# (-> m# :name)]
     (qualify-sym ns# name#)))

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
                  (and t (not f)) (constantly obj)
                  (and t-fn (not f)) (constantly obj)
                  :else (constantly f))]
        (fn' obj)))))

(def opae obj-pred-action-else)

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

(defn defalias-macro*
  [alias source]
  (let [body-sym (gensym "body")
        qualified-source (qualify-sym *ns* source)
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

(defn ns-unmap-all
  [ns']
  (->> ns'
       ns-map
       keys
       (map (partial ns-unmap ns'))
       dorun))

(defn macro?
  [v]
  (try (-> v
           (obj-pred-action-else symbol?
                                 :t-fn
                                 find-var)
           meta
           :macro)
       (catch java.lang.IllegalArgumentException e
         (macro? (qualify-sym 'clojure.core v)))))

(defn special-operator?
  [v]
  ((some-fn macro?
            special-symbol?)
   v))

(defn source-fn-var
  [fn-var]
  (->> fn-var
       meta
       ((juxt :ns
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
                           (drop (dec line))
                           (clojure.string/join "\n"))
                      "nil")))))

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

(defn deep-zipmap-no-colls
       [a b]
       (zipmap (filter (comp not coll?) (tree-seq coll? seq a))
               (filter (comp not coll?) (tree-seq coll? seq b))))


(defn deep-zipmap
       [a b]
       (zipmap (tree-seq coll? seq a)
               (tree-seq coll? seq b)))

(defn flatten-map-kv-pairs
  [m]
  (mapcat #(mapv
            vector
            (-> %
                first
                repeat)
            (second %))
          m))

(defmacro assoc-var-meta-to-fn
  [fn-sym meta-key]
  `(vary-meta ~fn-sym
              assoc
              ~meta-key
              (-> #'~fn-sym
                  meta
                  ~meta-key)))
