(ns sayid.trace
  "Outer tracing: wrapping a var so each call records its arguments, return or
  throw and timing into the active workspace's call tree.  Also home to the
  dynamic vars that bound the recording - `*record-limit*`, `*max-trace-depth*`,
  `*sample-rate*`, `*per-fn-limit*`, `*evict-old-calls*` - and the trace/untrace
  multimethods that inner tracing extends."
  (:require [sayid.util.other :as util]
            [sayid.util.sym :as sym])
  (:import sayid.SayidMultiFn))

(def ^:dynamic *trace-log-parent* nil)

(def ^:dynamic *record-limit*
  "The maximum number of top-level (root) calls a single workspace will record.
  Once the limit is reached, further top-level calls run untraced - the program
  behaves normally, the calls are just not captured - so tracing a namespace
  under a full test suite can't grow the recording without bound.  Rebind it or
  `alter-var-root` it to record more (or fewer)."
  50000)

(def ^:dynamic *max-trace-depth*
  "The deepest call nesting a workspace will record; nil means unbounded.  Calls
  below this depth (and everything under them) run untraced, so a deeply
  recursive function or an inner trace can't explode a single call into an
  unbounded subtree.  Root calls are depth 1."
  nil)

(def ^:dynamic *sample-rate*
  "Record one in every N top-level calls; 1 (the default) records them all.  Set
  higher to trace a hot entry point - say a request handler under load - and keep
  only a representative sample instead of drowning the recording."
  1)

(def ^:dynamic *suppress-recording*
  "When true, calls run untraced and nothing - outer or inner - is recorded.
  Bound around a call Sayid chose to skip (over a limit, sampled out, too deep),
  so the whole subtree under it is skipped instead of leaking in as spurious
  roots.  This is what lets root-level bounds compose correctly."
  false)

(def ^:dynamic *per-fn-limit*
  "The maximum number of calls of any single traced function a workspace will
  record; nil means unbounded.  Counts across the whole call tree, so once a hot
  function hits its limit, further calls of it (and everything under them) run
  untraced and it can't crowd out the rest of the recording."
  nil)

(def ^:dynamic *evict-old-calls*
  "How the workspace behaves at `*record-limit*` top-level calls.  When false (the
  default) it stops recording and keeps the *first* `*record-limit*` calls.  When
  true it keeps the *most recent* `*record-limit*` calls instead, evicting the
  oldest - handy when the interesting thing is what happened just before a
  failure at the end of a long run."
  false)

(defn- run-suppressed
  "Run `(apply F ARGS)` with recording suppressed for it and everything under it."
  [f args]
  (binding [*suppress-recording* true]
    (apply f args)))

(defonce ^:private record-limit-hit (atom false))

(defonce ^:private root-call-counter (atom 0))

(defonce ^:private per-fn-counts (atom {}))

(defonce ^:private per-fn-warned (atom #{}))

(defn reset-bounds!
  "Reset the per-workspace bound bookkeeping - the record-limit warning flag, the
  sampling counter, and the per-fn call counts.  Called when the log is cleared or
  the workspace reset."
  []
  (reset! record-limit-hit false)
  (reset! root-call-counter 0)
  (reset! per-fn-counts {})
  (reset! per-fn-warned #{}))

(defn- sample-root?
  "True when the current top-level call should be recorded under `*sample-rate*`."
  []
  (zero? (mod (swap! root-call-counter inc) *sample-rate*)))

(defn- over-per-fn-limit?
  "True when NAME has already been recorded `*per-fn-limit*` times."
  [name]
  (and *per-fn-limit*
       (>= (get @per-fn-counts name 0) *per-fn-limit*)))

(defn- bump-per-fn-count!
  "Count another recorded call of NAME when a per-fn limit is in effect."
  [name]
  (when *per-fn-limit*
    (swap! per-fn-counts update name (fnil inc 0))))

(defn- warn-per-fn-limit! [name]
  (when-not (contains? @per-fn-warned name)
    (swap! per-fn-warned conj name)
    (binding [*out* *err*]
      (println (str "Sayid: hit the " *per-fn-limit* "-call cap on " name
                    "; further calls of it run untraced.")))))

(defn- warn-record-limit! []
  (when (compare-and-set! record-limit-hit false true)
    (binding [*out* *err*]
      (println (str "Sayid: hit the " *record-limit* "-call recording limit; "
                    "further top-level calls run untraced.  Clear or reset the "
                    "workspace, or raise sayid.trace/*record-limit*, to record "
                    "more.")))))

(defn now
  "Current wall-clock time in milliseconds; stamps each node's start and end."
  [] (System/currentTimeMillis))

(defn mk-tree
  "Build a fresh call-tree node: a map with a unique :id, its :path from the root,
  :depth, and an empty :children atom.  PARENT supplies the path and depth to
  extend."
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

(defn- mk-fn-tree
  [& {:keys [parent name args meta]}]
  (assoc  (mk-tree :parent parent) ;; 3 sec
          :name name
          :args (vec args)
          :meta meta
          :arg-map (delay (util/arg-match-safe (-> meta
                                              :arglists
                                              vec)
                                          args))
          :started-at (now)))

(defn- StackTraceElement->map
  [^StackTraceElement o]
  {:class-name (.getClassName o)
   :file-name (.getFileName o)
   :method-name (.getMethodName o)
   :line-number (.getLineNumber o)})

(defn Throwable->map**
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

(defn- start-trace
  [trace-log tree]
  (swap! trace-log
         conj
         tree))  ;; string!!

(defn- end-trace
  [trace-log idx tree]
  (swap! trace-log
         update-in
         [idx]
         #(merge % tree)))

(defn- roots-index-by-id
  "Index of the root with :id ID in ROOTS, or nil when it's been evicted."
  [roots id]
  (loop [i 0]
    (cond (>= i (count roots)) nil
          (= (:id (nth roots i)) id) i
          :else (recur (inc i)))))

(defn- end-root-trace
  "Merge TREE into the root with :id ID.  Looked up by id, not position, so it's
  correct even when eviction has shifted the vector; a no-op if that root was
  already evicted."
  [ws-children id tree]
  (swap! ws-children
         (fn [roots]
           (if-let [i (roots-index-by-id roots id)]
             (update roots i #(merge % tree))
             roots))))

(defn- record-root-call-evicting
  "Record a top-level call while keeping only the most recent `*record-limit*`
  roots: append this call and drop the oldest beyond the limit.  Ends by id, so
  the index-shifting from eviction can't corrupt an in-flight call."
  [workspace name f args meta']
  (bump-per-fn-count! name)
  (let [this     (mk-fn-tree :parent workspace :name name :args args :meta meta')
        id       (:id this)
        children (:children workspace)]
    (swap! children
           (fn [roots]
             (let [roots' (conj roots this)
                   over   (- (count roots') *record-limit*)]
               (if (pos? over)
                 (subvec roots' over)
                 roots'))))
    (let [value (binding [*trace-log-parent* this]
                  (try
                    (apply f args)
                    (catch Throwable t
                      (end-root-trace children id
                                      {:throw (Throwable->map** t)
                                       :ended-at (now)})
                      (throw t))))]
      (end-root-trace children id
                      {:return value
                       :ended-at (now)})
      value)))

(defn- record-fn-call
  [parent name f args meta']
  (bump-per-fn-count! name)
  (let [this  (mk-fn-tree :parent parent
                          :name name
                          :args args
                          :meta meta')
        idx  (-> (start-trace (:children parent)
                              this)
                 count
                 dec)]
    (let [value (binding [*trace-log-parent* this]
                  (try
                    (apply f args)
                    (catch Throwable t
                      (end-trace (:children parent)
                                 idx
                                 {:throw (Throwable->map** t)
                                  :ended-at (now)})
                      (throw t))))]
      (end-trace (:children parent)
                 idx
                 {:return value
                  :ended-at (now)})
      value)))

(defn- trace-fn-call
  [workspace name f args meta']
  (if *suppress-recording*
    ;; We're inside a call we chose not to record - stay untraced.
    (apply f args)
    (let [parent (or *trace-log-parent* workspace)]
      (cond
        ;; This function has hit its per-fn cap - drop it and its subtree.
        (over-per-fn-limit? name)
        (do (warn-per-fn-limit! name)
            (run-suppressed f args))

        ;; Too deep - drop this call and, via the suppression flag, everything
        ;; under it, so a single call can't explode the recording.
        (and *max-trace-depth*
             (> (inc (:depth parent)) *max-trace-depth*))
        (run-suppressed f args)

        ;; Root-level bounds: 1-in-N sampling, then the global record limit.
        (nil? *trace-log-parent*)
        (cond
          (not (sample-root?))
          (run-suppressed f args)

          *evict-old-calls*
          (record-root-call-evicting workspace name f args meta')

          (>= (count @(:children workspace)) *record-limit*)
          (do (warn-record-limit!)
              (run-suppressed f args))

          :else
          (record-fn-call parent name f args meta'))

        ;; A nested call under a recorded root.
        :else
        (record-fn-call parent name f args meta')))))

(defn- shallow-tracer-multifn
  [{:keys [workspace qual-sym meta']} original-fn]
  (sayid.SayidMultiFn. {:original original-fn
                                     :trace-dispatch-fn (fn [f args]
                                                          (trace-fn-call workspace
                                                                         (symbol (str qual-sym "--DISPATCHER"))
                                                                         f
                                                                         args
                                                                         meta'))
                                     :trace-method-fn (fn [f args]
                                                        (trace-fn-call workspace
                                                                       qual-sym
                                                                       f
                                                                       args
                                                                       meta'))}))

(defn ^{::trace-type :fn} shallow-tracer
  "The outer tracer: wrap ORIGINAL-FN so each call records its arguments, return or
  throw and timing into the workspace.  M carries the workspace, the qualified
  symbol and the var's metadata.  Multimethods are wrapped specially."
  [{:keys [workspace qual-sym meta'] :as m} original-fn]
  (if (= (type original-fn) clojure.lang.MultiFn)
    (shallow-tracer-multifn m original-fn)
    (fn tracing-wrapper [& args]
      (trace-fn-call workspace
                     qual-sym
                     original-fn
                     args
                     meta'))))

(defn- apply-trace-to-var
  [^clojure.lang.Var v tracer-fn workspace]
  (let [ns (.ns v)
        s  (.sym v)
        m (meta v)
        f @v
        vname (sym/qualify-sym ns s )]
    (doto v
      (alter-var-root (partial tracer-fn
                               {:workspace workspace
                                :ns' ns
                                :sym s
                                :qual-sym vname
                                :meta' m}))
      (alter-meta! assoc ::traced [(:id workspace) f])
      (alter-meta! assoc ::trace-type (-> tracer-fn meta ::trace-type)))))

(defn untrace-var*
  "Restore a traced var to its original function, dropping Sayid's trace metadata.
  A no-op if the var isn't traced."
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
         (alter-meta! dissoc
                      ::traced
                      ::trace-type))))))

(defn trace-var*
  "Install TRACER-FN on var V for WORKSPACE.  Re-traces if V is already traced by a
  different workspace or trace type (unless :no-overwrite is set); skips macros and
  non-functions."
  [v tracer-fn workspace & {:keys [no-overwrite]}]
  (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
    (when (and (ifn? @v) (-> v meta :macro not))
      (if-let [[traced-id traced-f] (-> v meta ::traced)]
        (when (and (not no-overwrite)
                   (or (not= traced-id (:id workspace))
                       (not= (-> tracer-fn meta ::trace-type)
                             (-> v meta ::trace-type))))
          (untrace-var* v)
          (apply-trace-to-var v tracer-fn workspace))
        (apply-trace-to-var v tracer-fn workspace)))))

(defn- the-ns-safe
  [ns]
  (try (the-ns ns)
       (catch Exception e
         nil)))

(defn- trace-ns*
  [ns workspace]
  (when-let [ns (the-ns-safe ns)]
    (when-not ('#{clojure.core sayid.core} (ns-name ns))
      (let [ns-fns (->> ns ns-interns vals (filter (comp util/fn*? var-get)))]
        (doseq [f ns-fns]
          (trace-var* f
                      (util/assoc-var-meta-to-fn shallow-tracer
                                                 ::trace-type)
                      workspace
                      :no-overwrite true))))))

(defn untrace-ns*
  "Untrace every traced var in namespace NS*."
  [ns*]
  (when-let [ns' (the-ns-safe ns*)]
    (let [ns-fns (->> ns' ns-interns vals)]
      (doseq [f ns-fns]
        (untrace-var* f)))))

(defn- apply->vec
  [f]
  (fn [v] [v (f v)]))

(defn- audit-fn
  [fn-var trace-selection]
  (let [fn-meta (meta fn-var)]
    (-> fn-meta
        (update-in [:ns] str)
        (assoc :trace-type (::trace-type fn-meta)
               :trace-selection trace-selection)
        (dissoc ::trace-type
                ::traced))))

(defn audit-ns
  "A sorted map of every function in NS-SYM to its trace-audit info (its metadata
  plus current trace type) - the data behind reporting what's traced."
  [ns-sym]
  (try (let [mk-vec-fn (fn [fn-var]
                         [(-> fn-var meta :name)
                          (audit-fn fn-var :ns)])]
         (->> ns-sym
              ns-interns
              vals
              (filter (comp fn? var-get))
              (map mk-vec-fn)
              (into (sorted-map))))
       (catch Exception ex
         (sorted-map))))

(defn audit-traces
  "Summarize a workspace's TRACED selection as data: which namespaces are traced
  wholesale, and - grouped by namespace - each individually traced function with
  its trace type.  Backs the show-traced views."
  [traced]
  (let [{outer :fn inner :inner-fn ns' :ns} traced
        f (fn [trace-type]
            (fn [fn-sym]
              (let [fn-var (resolve fn-sym)]
                [(-> fn-var meta :name)
                 (audit-fn fn-var trace-type)])))
        fn-audits (->> (concat (map (f :fn) outer)
                               (map (f :inner-fn) inner))
                       (group-by #(-> % second :ns))
                       (map (fn [[k v]] [(symbol k) (into (sorted-map) v)]))
                       (into (sorted-map)))]
    {:ns (into (sorted-map)
               (map (apply->vec audit-ns)
                    ns'))
     :fn fn-audits}))

(defn check-fn-trace-type
  "The trace type currently applied to FN-SYM (`:fn`, `:inner-fn`, ...), or nil when
  it isn't traced."
  [fn-sym]
  (-> fn-sym
      resolve
      meta
      ::trace-type))

(defmulti trace* (fn [type sym workspace] type))

(defmethod trace* :ns
  [_ sym workspace]
  (trace-ns* sym workspace))

(defmethod trace* :fn
  [_ fn-sym workspace]
  (-> fn-sym
      resolve
      (trace-var* (util/assoc-var-meta-to-fn shallow-tracer
                                        ::trace-type)
                  workspace)))

(defmulti untrace* (fn [type sym] type))

(defmethod untrace* :ns
  [_ sym]
  (untrace-ns* sym))

(defmethod untrace* :fn
  [_ fn-sym]
  (-> fn-sym
      resolve
      untrace-var*))
