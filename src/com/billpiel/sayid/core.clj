(ns com.billpiel.sayid.core
  (:require com.billpiel.sayid.string-output
            [com.billpiel.sayid.trace :as trace]
            [com.billpiel.sayid.deep-trace2 :as dtrace]
            [com.billpiel.sayid.workspace :as ws]
            [com.billpiel.sayid.recording :as rec]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.util.find-ns :as find-ns]
            [com.billpiel.sayid.string-output :as so]
            [com.billpiel.sayid.profiling :as pro]
            [com.billpiel.sayid.util.other :as util]))

(def workspace
  "The activce workspace. Used by default in any function prefixed `ws-`
  or `w-`."
  (atom nil))

(def recording
  "The active recording. Used by default in any function prefixed `rec-`
  or `r-`."
  (atom nil))

(def config
  "Configuration map. Indicates which namespaces should be used to shelf
  workspaces and recordings."
  (atom {:ws-ns '$ws
         :rec-ns '$rec}))

(def ^:no-doc
  default-printer {:max-chars so/*max-chars*
                   :max-arg-lines so/*max-arg-lines*
                   :selector so/*selector*})

(def printer
  "The printer that is applied by `with-printer` and print functions
  such as `ws-print`."
  (atom default-printer))

(declare with-printer)
(declare ws-query*)
(declare ws-query)
(declare ws-print)
(declare trees-print)

;; === Helper functions

(defmacro src-in-meta
  "Takes a `body` form that evaluates to a var. Alters the var's meta
  to include the body source in :source. Useful for functions where
  source is not otherwise available -- ex eval'd outside the context of
  a file.

 Usage:

user> (sd/src-in-meta defn f1 [a] (inc a))
{:arglists ([a]), :line 1, :column 1, :file \"/tmp/form-init5170899558834081664.clj\", :name f1, :ns #object[clojure.lang.Namespace 0x7d351966 \"user\"], :source (defn f1 [a] (inc a))}

user> (-> #'f1 meta :source)
(defn f1 [a] (inc a))
"
  [& body]
  `(util/src-in-meta ~@body))

;; === Workspace functions

(defn- ws-init! [& [quiet]]
  (#'ws/init! workspace quiet))

(defn ws-get-active!
  "Returns the active workspace with atoms intact."
  []
  @(ws-init! :quiet))
(util/defalias w-ga! ws-get-active!)

(defn ws-show-traced*
  [& [ws]]
  (-> ws
      (or (ws-get-active!))
      :traced))

(defn ws-show-traced
  "Pretty prints the map that contains the traces that are currently in
  place."
  [& [ws]]
  (-> ws
      ws-show-traced*
      clojure.pprint/pprint))
(util/defalias w-st ws-show-traced)

(defn ws-remove-all-traces!
  "Disables and removes all traces in the active workspace."
  []
  (#'ws/remove-all-traces! workspace)
  (ws-show-traced))
(util/defalias w-rat! ws-remove-all-traces!)

(defn ws-remove-trace-fn!
  "Disables and removes a trace on a function from the active workspace."
  [fn-sym]
  (#'ws/remove-trace-fn! workspace fn-sym)
  (ws-show-traced))
(util/defalias w-rtf! ws-remove-trace-fn!)

(defn ws-reset!
  "Removes all traces set by active workspace. Resets the active workspace to nil."
  []
  (ws-remove-all-traces!)
  (#'ws/reset-to-nil! workspace))
(util/defalias w-rs! ws-reset!)

(defn ws-clear-log!
  "Clears the log of the active workspace, but preserves traces and other
  properties."
  [] (#'ws/clear-log! (ws-init! :quiet)))
(util/defalias w-cl! ws-clear-log!)

(defn ws-add-trace-fn!*
  [fn-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :fn
                      fn-sym)
  fn-sym)

(defmacro ws-add-trace-fn!
  "`fn-sym` is a symbol that references an existing function. Applies an
  enabled trace to said functions. Adds the traces to the active
  workspace trace set."
  [fn-sym]
  `(ws-add-trace-fn!* (util/fully-qualify-sym '~fn-sym)))
(util/defalias-macro w-atf! ws-add-trace-fn!)

(defn ^:no-doc ws-add-deep-trace-fn!*
  [fn-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :deep-fn
                     fn-sym)
  fn-sym)

(defmacro ws-add-deep-trace-fn!
  "`fn-sym` is a symbol that references an existing function. Applies an
  enabled *deep* trace to said functions. Adds the traces to the active
  workspace trace set. Deep traces capture all functions calls that
  occurr within the traced function."
  [fn-sym]
  `(ws-add-deep-trace-fn!* (util/fully-qualify-sym ~(util/quote-if-sym fn-sym))))

(util/defalias-macro w-adtf! ws-add-deep-trace-fn!)

(defn ^:no-doc ws-add-trace-ns!*
  "`ns-sym` is a symbol that references an existing namespace. Applies an enabled
  trace to all functions in that namespace. Adds the traces to the active workspace trace set."
  [ns-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :ns
                     ns-sym)
  ns-sym)

(defmacro ws-add-trace-ns!
  "`ns-sym` is a symbol that references an existing namespace. Applies an enabled
  trace to all functions in that namespace. Adds the traces to the active workspace trace set."
  [ns-sym]
  (let [ref-ns *ns*]
    `(mapv ws-add-trace-ns!* (find-ns/search-nses '~ns-sym ~ref-ns))))
(util/defalias-macro w-atn! ws-add-trace-ns!)

(defn ws-remove-trace-ns!
  "`ns-sym` is a symbol that references an existing namespace. Removes all
  traces applied to the namespace."
  [ns-sym] (#'ws/remove-trace-*! (ws-init! :quiet)
                                  :ns
                                  ns-sym))
(util/defalias w-rtn! ws-remove-trace-ns!)

(defn ws-enable-all-traces!
  "Enables any disabled traces in active workspace."
  [] (#'ws/enable-all-traces! workspace))
(util/defalias w-eat! ws-enable-all-traces!)

(defn ws-enable-trace-fn!
  "Enables a trace on a function. Function must already have trace added
  to active workspace."
  [fn-sym] (#'ws/enable-trace-fn! workspace
                                  fn-sym))
(util/defalias w-etf! ws-enable-trace-fn!)

(defn ws-disable-all-traces!
  "Disables all traces in active workspace. The active workspace trace set will be
  preserved and can be re-enabled."
  [] (#'ws/disable-all-traces! workspace))
(util/defalias w-dat! ws-disable-all-traces!)

(defn ws-disable-trace-fn!
  "Disables a trace on a function. The active workspace trace set will
  be preserved and can be re-enabled."
  [fn-sym] (#'ws/disable-trace-fn! workspace fn-sym))
(util/defalias w-dtf! ws-disable-trace-fn!)

(defn ws-cycle-all-traces!
  "Disables and enables all traces in active workspace. You shouldn't
  need to use this, but you might."
  []
  (ws-disable-all-traces!)
  (ws-enable-all-traces!))
(util/defalias w-cat! ws-cycle-all-traces!)

(defn ws-deref!
  "Returns the value of the active workspace, but with all children
  recursively dereferenced. This workspace value will not receive new
  trace entries."
  [& [w]] (#'ws/deep-deref! (or w
                                workspace)))
(util/defalias w-drf! ws-deref!)

(defn ws-save!
  "Saves active workspace to the workspace shelf namespace in the pre-specified slot."
  []
  (#'ws/save! workspace (:ws-ns @config)))
(util/defalias w-s! ws-save!)

(defn ws-save-as!
  "Saves active workspace to the workspace shelf namespace in the specified `slot`."
  [slot]
  (#'ws/save-as! workspace
                 (:ws-ns @config)
                 slot)
  true)
(util/defalias w-sa! ws-save-as!)

(defn ws-load!
  "Loads a workspace from the shelf namespace into the active
  position. Will not overwrite an un-saved active workspace unless
  `force` equals :f"
  [slot & [force]]
  (#'ws/load! workspace
              (:ws-ns @config)
              slot
              force)
  true)
(util/defalias w-l! ws-load!)

(defn ws-replay-id!
  "Replays the function call recorded in the active workspace with an id
  of `id`."
  [id]
  (let [t (-> (ws-query [:id id]) first)
        f (-> t :name resolve)
        a (:args t)]
    (apply f a)))
(util/defalias w-rpi! ws-replay-id!)

(defn ^:no-doc deep-trace-apply*
  [workspace qual-sym args]
  (let [meta' (-> qual-sym
                  resolve
                  meta)
        ns' (-> meta'
                :ns
                str)
        dtraced-fn (dtrace/deep-tracer {:workspace nil
                                        :qual-sym qual-sym
                                        :meta' meta'
                                        :ns' ns'}
                                       nil)]
    (binding [trace/*trace-log-parent* workspace]
      (apply dtraced-fn args))))

(defn ws-query-deep-trace-replay
  "Queries the current workspace with `query`. Then takes the top-level
  results, deep traces the functions and replays them. Returns the
  resulting workspace, which is NOT the active workspace."
  [query]
  (let [results (ws-query* query)
        workspace (ws/default-workspace)]
    (doseq [{:keys [name args]} results]
      (deep-trace-apply* workspace
                         name
                         args))
    (ws-deref! workspace)))
(util/defalias w-qdtr ws-query-deep-trace-replay)

(defn ws-query-deep-trace-replay-print
  "Queries the current workspace with `query`. Then takes the top-level
  results, deep traces the functions, replays them then PRINTS the
  resulting workspace, which is NOT the active workspace."
  [query]
  (-> query
      ws-query-deep-trace-replay
      trees-print
      with-printer))
(util/defalias w-qdtrp ws-query-deep-trace-replay-print)
(util/defalias w-$ ws-query-deep-trace-replay-print)

(defn ws-deep-trace-apply
  "Deep traces the function indicated by the qualified symbol,
  `qual-sym`, and then call it with arguments `args`. Returns the
  resulting workspace, which is NOT the active workspace."
  [qual-sym args]
  (let [workspace (ws/default-workspace)]
    (deep-trace-apply* workspace
                       qual-sym
                       args)
    (ws-deref! workspace)))
(util/defalias w-dta ws-deep-trace-apply)

(defn ws-deep-trace-apply-print
  "Run `ws-deep-trace-apply` then prints the resulting workspace."
  [qual-sym args]
  (ws-print (ws-deep-trace-apply qual-sym
                                  args)))
(util/defalias w-dtap ws-deep-trace-apply-print)


;; === END Workspace functions

;; === Recording functions

(defn rec-reset!
  "Removes all traces set by active workspace. Resets the active workspace to nil."
  []
  (#'rec/reset-to-nil! recording))
(util/defalias r-rst! rec-reset!)

(defn rec-save!
  "Saves active recording to the recording shelf namespace in the pre-specified slot."
  []
  (#'rec/save! recording (:rec-ns @config))
  true)
(util/defalias r-s! rec-save!)

(defn rec-save-as!
  "Saves active recording to the recording shelf namespace in the specified `slot`."
  [slot]
  (->>  (#'rec/save-as! recording
                        (:rec-ns @config)
                        slot)
        ((juxt :id :rec-slot))
        (apply format "Saved recording with id '%s' to slot '%s'.")))
(util/defalias r-sa! rec-save-as!)

(defn rec-load!
  "Loads a recording from the shelf namespace into the active
  position. Will not overwrite an un-saved active recording unless
  `active` equals :f"
  [slot & [force]]
  (#'rec/load! recording
              (:rec-ns @config)
              slot
              force))
(util/defalias r-l! rec-load!)

(defn rec-load-from!
  "Loads a recording from the provided source. Source may be a workspace"
  [src & [force]]
  (->> (#'rec/coerce&load! recording
                           src
                           (:rec-ns @config)
                           force)
       ((juxt :id :rec-slot))
       (apply format "Loaded recording with id '%s', slot '%s' to active position.")))
(util/defalias r-lf! rec-load-from!)

(defn rec-load-from-ws!
  "Loads the active workspace into the active record."
  [& [force]]
  (rec-load-from! (ws-get-active!) force)
  true)
(util/defalias r-lfw! rec-load-from-ws!)

;; === END Recording functions


;; === String Output functions

(def tree->string #'so/tree->string)

(defn- get-trees
  [v]
  (let [mk (meta v)]
    (cond
      (sequential? v)
      v

      ((some-fn ::ws/workspace
                ::rec/recording
                ::q/query-result)
       mk)
      (:children v)

      (::trace/tree mk)
      [v]

      (every? #(contains? v %)
              [:children :depth :args :name :return :arg-map :id])
      [v]

      :default
      (throw (Exception. (format "Don't know how to get a tree from this thing. keys=> %s, meta=> %s"
                                 (keys v)
                                 (meta v)))))))

(defn- mk-printer-*-list
  [opts init-prn whitelist?]
  (loop [prn init-prn
         opts' opts]
    (let [[fi se & re] opts'
          limit ({:mc :max-chars
                  :mal :max-arg-lines} fi)]
      (cond
        (nil? fi) prn
        limit (recur (assoc prn limit se)
                     re)
        (map? fi) (recur (update-in prn
                                    [:selector
                                     :selects]
                                    merge
                                    fi)
                         (rest opts'))
        :else (recur (update-in prn [:selector]
                                assoc fi whitelist?)
                     (rest opts'))))))

(defn- mk-printer-white-list
  [opts]
  (mk-printer-*-list opts
                     (assoc default-printer
                            :selector {})
                     true))

(defn- mk-printer-black-list
  [opts]
  (mk-printer-*-list opts
                     default-printer
                     false))

(defn mk-printer
  [opts]
  (cond
    (nil? opts) default-printer

    (-> opts first (= :-))
    (mk-printer-black-list (rest opts))

    :else (mk-printer-white-list opts)))

(defmacro with-this-printer
  "Sets the printer as `prn` for the lexical scope. `prn` may be a
  printer map or a vector. See `(doc mk-printer)` for details on the
  latter."
  ([prn & body]
   `(let [prn# (if (vector? ~prn)
                 (mk-printer ~prn)
                 ~prn)]
      (binding [so/*max-chars* (or (:max-chars prn#)
                                   so/*max-chars*)
                so/*max-arg-lines* (or (:max-arg-lines prn#)
                                       so/*max-arg-lines*)
                so/*selector* (or (:selector prn#)
                                  so/*selector*)]
        ~@body))))

(defmacro with-printer
  "Puts the printer in effect for the lexical scope."
  ([& body]
   `(with-this-printer @printer ~@body)))

(defn trees-print
  "Prints `trees`, which may be either trace tree, a collection of trace
  trees, or a known structure (workspace, recording) that contains a trace tree."
  [trees]
  (-> trees
      get-trees
      (#'so/print-trees)))
(util/defalias t-pr trees-print)

(defn ws-print-unlimited
  "Prints either the active workspace, or the first argument, but
  without any restrictions on the output. Don't blow up your buffer!"
  [& [ws]]
  (#'so/print-tree-unlimited (or ws
                       (ws-deref!))))
(util/defalias w-pru ws-print-unlimited)

(defn ws-print
  "Prints either the active workspace, or the first argument, using the
  default printer, which puts safety restrictions on the output to
  prevent overwhelming hugeness."
  [& [ws]]
  (with-printer (#'so/print-tree (or ws
                                     (ws-deref!)))))
(util/defalias w-pr ws-print)

(defn rec-print
  [& [rec]]
  (#'so/print-tree (or rec
                       @recording)))
(util/defalias r-pr rec-print)

(defn set-printer!
  "Sets the printer that is applied by `with-printer` and used by print
  functions such as `ws-print`."
  [& opts]
  (reset! printer
          (mk-printer opts)))

;; === END String Output functions


;; === Query functions

(def query-docs
  "There are several querying functions. Many of them take a variadic
  `body` argument. The syntax of the `body` argument is described
  below:

  Body may or may not begin with a keyword. If it doesn't, body is one
  or more vectors which specify a query.

  If it does being with a keyword, the syntax rules of that keyword
  apply to the args that follow. The keyword acts as a modifier that
  expands the query results to include nodes that have a specified type
  of relationship with any nodes matching the query. The modifiers are
  listed here:

  :a -- returns ancestors of matching nodes
  :d -- returns descendants of matching nodes
  :s -- returns siblings of matching nodes
  :w -- wildcard! returns ancestors, descendants and siblings of matching nodes
  :r -- range; takes exactly two query vectors and returns nodes that
        are both descendants of the first and ancestors of the second

  Additionally, the keywords :a, :d, :s and :w take an optional numeric
  argument which precedes the vector queries. This number specifies a
  limit on the number of relative hops that will be taken.

  A query clause may be a vector or a symbol. A vector is applied in a
  `get-in` fashion to each trace node, with the final element acting as
  a matching value or predicate function. If the final value is truthy,
  the node is included in the query result set. For example, a `body` of

  :a 2 [:arg-map 'fruit :apple]

  would match any trace node where an argument `fruit` took a
  value :apple, as well as the parent and grandparent of that node.

  If the query is a symbol instead of a vector, the query will match any
  node whose function name matches the symbol. For example, a `body` of

  somefunc

  is equivalent to: [:name 'somefunc]
"
  nil)

(defmacro query-by-name
  "Produces a vector like [:name 'func] for `s` equal to 'func. Just a
  little shortcut for when querying by a function name."
  [s]
  `[:name '~(util/fully-qualify-sym s)])
(util/defalias-macro qbn query-by-name)

(defn- syms->qbn
  [form]
  (map #(if (symbol? %)
          `(qbn ~%)
          %)
       form))

(defn ws-query*
  [& query]
  (apply q/q
         (ws-deref!)
         query))

(defmacro ws-query
  "Queries the trace record of the active workspace."
  [& body] `(ws-query* ~@(syms->qbn body)))
(util/defalias-macro w-q ws-query)

(defmacro ws-query-print
  "Queries the trace record of the active workspace and prints the results."
  [& body]
  `(with-printer
     (trees-print (ws-query ~@body))))
(util/defalias-macro w-qp ws-query-print)
(util/defalias-macro q ws-query-print)

(defmacro rec-query
  "Queries the active trace recording."
  [& body] `(q/q @recording
                 ~@body))
(util/defalias-macro r-q rec-query)

(defmacro tree-query
  "Queries `tree`, a trace tree."
  [tree & body] `(q/q ~tree
                      ~@body))
(util/defalias-macro t-query tree-query)

;; === END Query functions

;; === Profiling functions

(defn pro-analyze
  "Takes a tree (workspace, recording, query result) and assocs profile
  data to it at :profile."
  [tree]
  (#'pro/assoc-tree-with-profile tree))
(util/defalias p-a pro-analyze)

(defn pro-net-time
  "Takes a tree with profilings data (see `pro-analyze`) and prints a
  table of functions and their profile metrics, sorted by net time
  sum. 'Net time sum' is the amount of time spent in a function minus
  the time spent executing its children. Functions with high net time
  sum may be candidates for optimization."
  [tree]
  (->> tree
       :profile
       (map (fn [[k v]]
              (assoc v
                     :name k)))
       (sort-by :net-time-sum)
       (clojure.pprint/print-table [:name :net-time-sum
                                    :net-time-avg :count
                                    :gross-time-sum :gross-time-avg])))
(util/defalias p-nt pro-net-time)


(defn pro-gross-repeats
  "Takes a tree with profilings data (see `pro-analyze`) and prints a
  table of functions and their profile metrics, sorted by gross time of
  repeated arguments. 'Gross of repeats' is the amount of time spent in
  a function during calls where the args match those of a previous call
  to the function. Functions with high gross of repeats may be
  candidates for memoization."
  [tree]
  (->> tree
       :profile
       (map (fn [[k v]]
              (assoc v
                     :name k)))
       (sort-by :gross-of-repeats)
       (clojure.pprint/print-table [:name :gross-of-repeats
                                    :count :arg-cardinality
                                    :repeat-arg-pct
                                    :gross-time-sum :gross-time-avg])))
(util/defalias p-gr pro-gross-repeats)

;; === END Profiling functions


;; === TEMP

(defn mk-src-pos-query-fn
  [file line]
  (fn [{:keys [src-pos]}]
    (and (= (:file src-pos) file)
         (<= (:line src-pos) line)
         (>= (:end-line src-pos) line))))

(defn ws-query-by-file-line
  [file line]
  (println)
  (trees-print (ws-query* [(mk-src-pos-query-fn file line)]))
  (println))

(defn ws-query-by-file-pos
  [file line]
  (ws-query* [:id (q/get-ids-from-file-pos (ws-deref!)
                                           file
                                           line)]))
