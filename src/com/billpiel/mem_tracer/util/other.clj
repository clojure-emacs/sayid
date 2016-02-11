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

(defn arg-matcher-fn
  [arglists]
  (when (not-empty arglists)
    (let [arities (map #(list % '(com.billpiel.mem-tracer.util.other/get-env)) ;; NOTE!  NS/get-env must match this ns
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
  (let [body-sym (gensym "body")
        qualified-source (qualify-sym *ns* source)
        source-meta (->> source
                         (ns-resolve *ns*)
                         meta)
        arglists (:arglists source-meta)
        forms (map (fn [args]
                     `(~args
                       (clojure.core/seq
                        (clojure.core/concat
                         (clojure.core/list '~qualified-source)
                         ~args))))
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

(defn back-into
  [orig noob]

  (let [r ((if (seq? orig)
             reverse
             identity)
           (into  (or (empty orig)
                      [])
                  noob))
        ]
    r))

(defn swap-in-path-syms
  [form & [path]]
  (cond
    (special-operator? form) form
    (coll? form)  (back-into form (doall (map-indexed #(swap-in-path-syms %2
                                                                          (conj (or path
                                                                                    [])
                                                                                %))
                                                      form)))
    :else (symbol (apply str
                         "___"
                         (clojure.string/join "-"
                                              path)))))

(defn deep-zipmap
       [a b]
       (zipmap (filter (comp not coll?) (tree-seq coll? seq a))
               (filter (comp not coll?) (tree-seq coll? seq b))))

#_ (do
     (clojure.walk/macroexpand-all
      (swap-in-path-syms
       '(defn f1 [a] (let [b 4] {a (-> a (+ b))})))

      )



     (def src '(defn f1 [a] (let [[b] [4 5]] {a (-> a (+ b) dec)})))
     '(def f1 (fn* ([a]
                    (let* [vec__21418 [4 5]
                           b (clojure.core/nth vec__21418 0
                                               nil)]
                          {a (dec (+ a
                                     b))}))))

     {[2 1 1 1 3 0] {:type :expr
                     :provides [3 1 0 0] ; some info about mapping to '[4 5]  ??
                     :coordinates [2 1 1 1 3 0]
                     :expanded '(clojure.core/nth vec__21418 0 nil)}
      [2 1 1 2 0 1 0] {:type :expr
                       :provides [3 2 0 1 3]
                       :coordinates [2 1 1 2 0 1 0]
                       :original '(-> a (+ b) dec)
                       :expanded '(dec (+ a b))}
      [2 1 1 2 0 1 1 0] {:type :expr
                         :provides [3 2 0 1 2 0]
                         :coordinates [2 1 1 2 0 1 1 0]
                         :original '(+ b)
                         :expanded '(+ a b)}}

     '(def f1_1234 (fn* ([a]
                         (let* [vec__21418 [4 5]
                                b ((f* nth [2 1 1 1 3 0])
                                   vec__21418 0
                                   nil)]
                               {a ((f* dec [2 1 1 2 0 1 0])
                                   ((f* + [2 1 1 2 0 1 1 0])
                                    a b))}))))

     (deep-zipmap (clojure.walk/macroexpand-all
                   (swap-in-path-syms src))
                  (swap-in-path-syms
                   (clojure.walk/macroexpand-all src)))

     (swap-in-path-syms
      (clojure.walk/macroexpand-all src)))
