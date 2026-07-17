(ns sayid.nrepl-middleware
  "The nREPL wire protocol.  `wrap-sayid` registers Sayid's nREPL ops - trace
  management, queries, and the data ops that hand the recorded call tree to editor
  clients as plain data (see doc/nrepl-api.md) - by collecting the `^:nrepl`-tagged
  vars in this namespace."
  (:require
   [clojure.stacktrace :as st]
   clojure.string
   clojure.walk
   [clojure.tools.namespace.find :as ns-find]
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rts]
   [sayid.core :as sd]
   [sayid.data :as sd-data]
   [sayid.golden :as gold]
   [sayid.query :as q]
   [sayid.string-output :as so]
   [sayid.trace :as tr]
   [sayid.util.find-ns :as find-ns]
   [sayid.util.other :as util]
   [sayid.util.sym :as sym]
   [sayid.view :as v]
   [nrepl.middleware :refer [set-descriptor!]]
   [nrepl.misc :refer [response-for]]
   [nrepl.transport :as t]
   [tamarin.core :as tam]))

(def views (atom {}))
(def selected-view (atom nil))

(defn- try-find-ns-root
  [ns-sym]
  (let [depth (some-> ns-sym str (clojure.string/split #"\.") count)]
    (util/$- some-> ns-sym ns-interns vals first meta :file (clojure.string/split #"/") (drop-last depth $) (clojure.string/join "/" $))))

(defn- find-all-ns-roots
  []
  (some->> (all-ns) (map str) (map symbol) (map try-find-ns-root) distinct (remove empty?)))

(defn- query*
  [& args]
  (if args
    (assoc (apply sd/ws-query*
                  args)
           ::query-args
           args)
    (assoc (sd/ws-view!)
           ::query-args
           nil)))

(defn- query-tree->trio
  [tree]
  (conj (vec (so/tree->text-prop-pair tree))
        (-> tree ::query-args pr-str)))

(defn- clj->nrepl*
  [v]
  (cond (coll? v) (list* v)
        (number? v) v
        (keyword? v) (name v)
        (true? v) 1
        (false? v) nil
        :else (str v)))

(defn clj->nrepl
  [frm]
  (clojure.walk/prewalk clj->nrepl*
                        frm))

(defn- send-status-done
  [msg]
  (t/send (:transport msg)
          (response-for msg :status :done)))

(defn- reply:clj->nrepl
  [msg out]
  (try (t/send (:transport msg)
               (response-for msg
                             :value (clj->nrepl out)))
       (catch Exception e
         (println "EXCEPTION!")
         (println e)))
  (send-status-done msg))

(defn- reply:data
  "Reply with OUT sent as-is - no `clj->nrepl` flattening - and end the op.  For
  the data ops, whose shapes are already built to round-trip over bencode."
  [msg out]
  (t/send (:transport msg)
          (response-for msg :value out))
  (send-status-done msg))

(defn- reply:error
  "Reply to MSG with a CIDER-renderable error response for EX and end the op.
Mirrors the `:err'/`:ex' plus `:error'/`:done' status convention used by
cider-nrepl, so the client surfaces the failure instead of silently getting
no value."
  [msg ex]
  (t/send (:transport msg)
          (response-for msg
                        :status #{:error :done}
                        :ex (str (class ex))
                        :err (with-out-str (st/print-cause-trace ex)))))

(defn- find-ns-sym
  [file]
  (some->> file
           slurp
           (re-find #"\(ns\s+(.+)\b")
           second
           symbol))

(defn- get-top-form-at-pos-in-source
  [file line source]
  (let [rdr (rts/source-logging-push-back-reader source
                                                 (count source)
                                                 file)]
    (loop [rdr-iter rdr]
      (let [frm (r/read rdr)
            m (meta frm)]
        (cond
          (nil? frm) nil
          (<= (:line m) line (:end-line m)) frm
          (> (:line m) line) nil
          :else (recur rdr-iter))))))

(defn- get-meta-at-pos-in-source
  [file line source]
  (meta (get-top-form-at-pos-in-source file line source)))

(defn- pos-inside-line-column?
  [pos-line pos-column start-line end-line start-col end-col]
  (if (= start-line pos-line end-line)
    (<= start-col pos-column end-col)
    (or (and (= start-line pos-line)
             (<= start-col pos-column))
        (and (= end-line pos-line)
             (< pos-column end-col))
        (< start-line pos-line end-line))))

(defn- get-sym-at-pos-in-source
  [file pos-line pos-column source]
  (let [tseq (tree-seq coll? seq (get-top-form-at-pos-in-source file pos-line source))
        inside? (fn [sym]
                  (let [{:keys [line end-line column end-column]} (meta sym)]
                    (when line
                      (pos-inside-line-column? pos-line
                                               pos-column
                                               line
                                               end-line
                                               column
                                               end-column))))]
    (last (filter inside? tseq))))

(defn- parse-ns-name-from-source ;; TODO don't use this
  [source]
  (second (re-find #"\(\s*ns\s+([\w$.*-]+)"
                   source)))

(defn ^:nrepl sayid-version
  [msg]
  (reply:clj->nrepl msg sd/version))

(def fn-trace-actions
  "Maps a trace `action' string to the workspace function that applies it to a
qualified function symbol."
  {"add-outer" sd/ws-add-trace-fn!*
   "add-inner" sd/ws-add-inner-trace-fn!*
   "enable"    sd/ws-enable-trace-fn!
   "disable"   sd/ws-disable-trace-fn!
   "remove"    sd/ws-remove-trace-fn!})

(defn- fn-traced?
  "True when QUAL-SYM is currently traced - individually or via its namespace."
  [qual-sym]
  (let [{ns-syms :ns fn-syms :fn inner-syms :inner-fn}
        (:traced (sd/ws-get-active!))]
    (boolean (or (contains? fn-syms qual-sym)
                 (contains? inner-syms qual-sym)
                 (contains? ns-syms (symbol (namespace qual-sym)))))))

(defn ^:nrepl sayid-trace-fn-at-point
  "Apply trace ACTION to the function whose symbol sits at the cursor position
described by MSG (file/line/column/source).  Replies with a map of the resolved
symbol (`sym`) and whether it was traced before the action (`was-traced`, 0/1),
so the client can describe the outcome truthfully - or an empty map when no
function resolves there.  Enable/disable/remove are skipped (but still
reported) when the function isn't traced, instead of silently no-oping."
  [{:keys [action file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym (sym/resolve-to-qual-sym ns-sym sym)]
    (if-not qual-sym
      (reply:data msg {})
      (let [was-traced (fn-traced? qual-sym)]
        (when (or was-traced (#{"add-outer" "add-inner"} action))
          ((fn-trace-actions action) qual-sym))
        (reply:data msg {"sym"        (str qual-sym)
                         "was-traced" (if was-traced 1 0)})))))

;; ======================

(defn ^:nrepl sayid-set-view
  [{:keys [transport view-name] :as msg}]

  (-> view-name keyword (@views) sd/set-view!)
  (reset! selected-view @sd/view)
  (send-status-done msg))

(defn ^:nrepl sayid-toggle-view
  [{:keys [transport] :as msg}]
  (if (and @selected-view
           (not= @sd/view @selected-view))
    (do (sd/set-view! @selected-view)
        (reply:clj->nrepl msg 1))
    (do (sd/set-view!)
        (reply:clj->nrepl msg 0)))
  (send-status-done msg))

(defn ^:nrepl sayid-get-views
  [{:keys [transport source file line] :as msg}]
  (reply:clj->nrepl msg (keys @views)))

(defn ^:nrepl sayid-get-meta-at-point
  [{:keys [transport source file line] :as msg}]
  (t/send transport
          (response-for msg
                        :value (str (get-meta-at-pos-in-source file line source))))
  (send-status-done msg))

(defn ^:nrepl sayid-show-traced
  [{:keys [ns] :as msg}]
  (reply:clj->nrepl msg
                    (so/audit->text-prop-pair (-> @sd/workspace :traced tr/audit-traces)
                                              ns)))

(defn- audit-fn->data
  "Serialize one fn-info map from the traced audit to bencode data."
  [fn-info]
  (->> {"name"       (some-> (:name fn-info) str)
        "ns"         (:ns fn-info)
        "file"       (:file fn-info)
        "line"       (:line fn-info)
        "trace-type" (some-> (:trace-type fn-info) name)}
       (filter (comp some? val))
       (into {})))

(defn audit->data
  "Serialize a traced-fns AUDIT (from `trace/audit-traces`) to bencode data: a
  list of namespace groups, each with the traced fns in it.  Merges the fns
  traced by namespace and the ones traced individually."
  [audit]
  (mapv (fn [[ns-sym fns]]
          {"ns"  (str ns-sym)
           "fns" (mapv audit-fn->data (vals fns))})
        (merge-with merge (:ns audit) (:fn audit))))

(defn ^:nrepl sayid-show-traced-data
  "Return what Sayid has traced as data - a list of namespace groups, each with
  its traced functions - for clients that render it themselves.  With a non-empty
  NS, restrict to that namespace.  The rendered counterpart is `sayid-show-traced`."
  [{:keys [ns] :as msg}]
  (let [groups (audit->data (-> @sd/workspace :traced tr/audit-traces))]
    (reply:data msg (if (not-empty ns)
                      (filterv #(= ns (get % "ns")) groups)
                      groups))))

(defn- count-traces
  [trace-audit]
  (+ (count  (for [v1 (-> trace-audit :ns vals)
                   v2 (vals v1)]
               v2))
     (count  (for [v1 (-> trace-audit :fn vals)
                   v2 (vals v1)]
               v2))))

(defn- count-enabled-traces
  [trace-audit]
  (+ (count  (for [v1 (-> trace-audit :ns vals)
                   v2 (vals v1)
                   :when (-> v2 :trace-type nil? not)]
               v2))
     (count  (for [v1 (-> trace-audit :fn vals)
                   v2 (vals v1)
                   :when (-> v2 :trace-type nil? not)]
               v2))))

(defn ^:nrepl sayid-get-trace-count
  [{:keys [transport] :as msg}]
  (util/$- -> @sd/workspace
           :traced
           tr/audit-traces
           count-traces
           (reply:clj->nrepl msg $)))

(defn ^:nrepl sayid-get-enabled-trace-count
  [{:keys [transport] :as msg}]
  (util/$- -> @sd/workspace
           :traced
           tr/audit-traces
           count-enabled-traces
           (reply:clj->nrepl msg $)))

(defn ^:nrepl sayid-get-log-count
  "Reply with the number of top-level calls currently recorded in the workspace."
  [msg]
  (reply:clj->nrepl msg (count (:children (sd/ws-deref!)))))

(defn ^:nrepl sayid-trace-fn
  "Apply trace ACTION to the function named by MSG's fn-ns/fn-name."
  [{:keys [action fn-name fn-ns] :as msg}]
  ((fn-trace-actions action) (sym/qualify-sym fn-ns fn-name))
  (send-status-done msg))

;; `ws-remove-trace-ns!' is a macro that captures its argument's literal form,
;; so the namespace actions can't be table-driven the way the fn ones are.
(defn ^:nrepl sayid-trace-ns
  "Apply trace ACTION (enable/disable/remove) to the namespace named by MSG's ns."
  [{:keys [action ns] :as msg}]
  (case action
    "enable"  (sd/ws-enable-trace-ns! (symbol ns))
    "disable" (sd/ws-disable-trace-ns! (symbol ns))
    "remove"  (sd/ws-remove-trace-ns! (symbol ns)))
  (send-status-done msg))

(defn ^:nrepl sayid-query-form-at-point
  [{:keys [file line] :as msg}]
  (reply:clj->nrepl msg
                    (-> (sd/ws-query-by-file-pos file line)
                        so/tree->text-prop-pair)))

(defn- buf-query-tree
  "Build and run a buffer query.  Q-VEC is the base query; MOD-STR an optional
  modifier like \"a\" (ancestors) or \"d3\" (descendants, depth 3).  Returns the
  result tree, which `query-tree->trio` renders or `query-tree->data` turns into
  data."
  [q-vec mod-str]
  (let [[_ sk sn] (re-find #"(\w+)\s*(\d+)?" (or mod-str ""))
        k (keyword sk)
        n (util/->int sn)
        query (remove nil? [k n q-vec])]
    (apply query* query)))

(defn- sayid-buf-query
  [q-vec mod-str]
  (sd/with-view (query-tree->trio (buf-query-tree q-vec mod-str))))

(defn ^:nrepl sayid-query-by-id
  [{:keys [trace-id mod] :as msg}]
  (reply:clj->nrepl msg
                    (sayid-buf-query [:id (keyword trace-id)]
                                     mod)))

(def ^:private parent-name-or-name (some-fn :parent-name :name))

(defn ^:nrepl sayid-query-by-fn
  [{:keys [fn-name mod] :as msg}]
  (reply:clj->nrepl msg (sayid-buf-query [#'parent-name-or-name
                                          (symbol fn-name)]
                                         mod)))

;; this func is unfortunate
(defn- str-vec->arg-path
  [[kw & idx]]
  (let [kw' (keyword kw)
        str->sym (fn [s] (if (string? s)
                           (symbol s)
                           s))]
    (into [kw'] (mapv str->sym idx))))

;; ===== gen-instance-expr helpers

(defn- find-arg-list-by-length
  [n [first-list & rest-lists]]
  (let [cfl (count first-list)]
    (cond (nil? first-list) nil

          (or (= n cfl)
              (and (-> first-list reverse rest first (= '&))
                   (>= n (dec cfl))))
          first-list

          :else (recur n rest-lists))))

(defn- get-args-sym-template
  [arglist]
  (let [convert-non-syms (fn [v] (if (symbol? v)
                                   v
                                   '*))]
    (util/$- ->> arglist
             (remove #{'&})
             (map convert-non-syms)
             (concat $ (repeat (last $))))))

(defn- find-available-sym
  [ns-sym prefix & [init-taken]]
  (let [taken (set (or init-taken
                       (-> ns-sym
                           create-ns
                           ns-interns
                           keys)))]
    (loop [n 0]
      (let [suffix (if (= n 0) "" n)
            candidate (symbol (str prefix suffix))]
        (if-not (taken candidate)
          candidate
          (recur (inc n)))))))

(defn- lazy-find-available-sym
  [prefix-seq init-taken]
  (let [next (find-available-sym nil (first prefix-seq) init-taken)]
    (lazy-cat [next]
              (lazy-find-available-sym (rest prefix-seq)
                                       (conj init-taken next)))))

(defn- mk-avail-sym-lazy-seq
  [n arglists]
  (lazy-find-available-sym (get-args-sym-template (find-arg-list-by-length n
                                                                           arglists))
                           (-> '$s
                               create-ns
                               ns-interns
                               keys
                               (or []))))

;; END ===== gen-instance-expr helpers

(defn- gen-instance-expr
  [tree]
  (let [arg-count (-> tree :args count)
        arglists (-> tree :meta :arglists)
        arglist-template-seq (mk-avail-sym-lazy-seq arg-count
                                                    arglists)]
    (doseq [pair (map vector
                      arglist-template-seq
                      (:args tree))]
      (apply sym/def-ns-var
             '$s
             pair))
    (format "(%s%s)"
            (-> tree :meta :name)
            (apply str (interleave (repeat " $s/")
                                   (take arg-count arglist-template-seq))))))

(defn ^:nrepl sayid-find-all-ns-roots
  [{:keys [transport] :as msg}]
  (reply:clj->nrepl msg (find-all-ns-roots)))

(defn ^:nrepl sayid-gen-instance-expr
  [{:keys [transport trace-id] :as msg}]
  (or (some->> (sd/ws-query* [:id (keyword trace-id)])
            :children
            first
            gen-instance-expr
            (reply:clj->nrepl msg))
      (send-status-done msg)))

(defn ^:nrepl sayid-def-value
  [{:keys [transport trace-id path] :as msg}]
  (let [path' (str-vec->arg-path path)]
    (sym/def-ns-var '$s '* (-> [:id (keyword trace-id)]
                                sd/ws-query*
                                :children
                                first
                                (get-in path'))))
  (t/send transport (response-for msg :value "Def'd as $s/*"))
  (send-status-done msg))

(defn ^:nrepl sayid-pprint-value
  [{:keys [transport trace-id path] :as msg}]
  (let [path' (str-vec->arg-path path)
        value (-> [:id (keyword trace-id)]
                  sd/ws-query*
                  :children
                  first
                  (get-in path'))]
    (binding [tam/*max-y* 5000
              tam/*max-seq-items* 100]
      (->> value
           so/value->text-prop-pair*
           (reply:clj->nrepl msg)))))

(defn ^:nrepl sayid-clear-log
  [{:keys [transport] :as msg}]
  (sd/ws-clear-log!)
  (send-status-done msg))

(defn ^:nrepl sayid-reset-workspace
  [{:keys [transport] :as msg}]
  (sd/ws-reset!)
  (send-status-done msg))

(defn ^:nrepl sayid-tap-trace
  "tap> the recorded workspace as data, for exploring in Portal/Reveal/Morse."
  [msg]
  (reply:clj->nrepl msg (str "Tapped " (sd-data/tap-trace!) " call(s).")))

(defn ^:nrepl sayid-capture-baseline
  "Snapshot the current trace as the baseline for `sayid-diff-traces`."
  [msg]
  (reply:clj->nrepl msg (str "Captured baseline: " (gold/capture-baseline!)
                             " call(s).")))

(defn ^:nrepl sayid-diff-traces
  "Diff the current trace against the captured baseline; tap the diff for
  exploration and reply with a short summary."
  [msg]
  (let [d (gold/diff-from-baseline)]
    (reply:clj->nrepl
     msg
     (cond
       (nil? d)   "No baseline captured yet - use sayid-capture-baseline first."
       (empty? d) "Traces are identical."
       :else      (do (tap> d)
                      (str (count d) " root call(s) differ; tapped the diff."))))))

(defn ^:nrepl sayid-trace-all-ns-in-dir
  [{:keys [transport dir] :as msg}]
  (doall (map sd/ws-add-trace-ns!*
              (ns-find/find-namespaces-in-dir (java.io.File. ^String dir))))
  (sd/ws-cycle-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-by-pattern
  [{:keys [transport ns-pattern ref-ns] :as msg}]
  (mapv #(-> %
             str
             symbol
             sd/ws-add-trace-ns!*)
        (find-ns/search-nses (symbol ns-pattern)
                             (symbol ref-ns)))
  (sd/ws-cycle-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-in-file
  [{:keys [transport file] :as msg}]
  (->> file
       find-ns-sym
       sd/ws-add-trace-ns!*)
  (send-status-done msg))

(defn ^:nrepl sayid-all-traces
  "Apply trace ACTION (enable/disable/remove) to every trace at once."
  [{:keys [action] :as msg}]
  (case action
    "enable"  (sd/ws-enable-all-traces!)
    "disable" (sd/ws-disable-all-traces!)
    "remove"  (sd/ws-remove-all-traces!))
  (send-status-done msg))

(defn ^:nrepl sayid-get-workspace
  [msg]
  (reply:clj->nrepl msg
                    (sd/with-this-view (or @sd/view
                                      (v/mk-simple-view {}))
                      (query-tree->trio (sd/ws-view!)))))

(def ^:dynamic *value-print-length*
  "`*print-length*` applied when serializing captured values in the data ops, so
  a fat or infinite value produces a bounded string instead of hanging the
  serializer.  nil means unbounded."
  100)

(def ^:dynamic *value-print-level*
  "`*print-level*` applied when serializing captured values in the data ops."
  10)

(defn- pr-value
  "`pr-str` a captured value with the data-op print bounds applied, so an
  arbitrarily large or lazy/infinite value can't blow up the wire payload or hang
  the serializer."
  [v]
  (binding [*print-length* *value-print-length*
            *print-level* *value-print-level*]
    (pr-str v)))

(defn- throw->data
  "Trim a captured `:throw` (a `Throwable->map`) to the bencode-friendly bits a
  client needs: the message, the exception class, and any ex-data.  The full
  stack trace stays out of the wire shape."
  [thrown]
  (->> {"cause" (:cause thrown)
        "class" (some-> thrown :via first :type str)
        "data"  (some-> thrown :data pr-value)}
       (filter (comp some? val))
       (into {})))

(defn node->data
  "Serialize one recorded call-tree NODE into a bencode-friendly map.  Structural
  fields stay native (strings, ints, nested maps and lists) so a client can
  navigate them directly; the arbitrary captured values - args, arg-map and the
  return (or throw) - are `pr-str`'d, since they can't round-trip as data.  See
  doc/nrepl-api.md for the documented shape."
  [node]
  (let [m (:meta node)
        base {"id"         (some-> (:id node) name)
              "name"       (some-> (:name node) str)
              "form"       (some-> (:form node) pr-str)
              "depth"      (:depth node)
              "started-at" (:started-at node)
              "ended-at"   (:ended-at node)
              "args"       (mapv pr-value (:args node))
              "arg-map"    (reduce-kv (fn [acc k v]
                                        (assoc acc (str k) (pr-value v)))
                                      {} (:arg-map node))
              "file"       (:file m)
              "line"       (:line m)
              "children"   (mapv node->data (:children node))}
        ;; A node carries a `:throw` key only when it actually threw; on an
        ;; inner-trace node the key is always present but nil, so test the value,
        ;; not its presence, or every successful inner call looks like a throw.
        outcome (if (not-empty (:throw node))
                  {"throw" (throw->data (:throw node))}
                  {"return" (pr-value (:return node))})]
    (->> (merge base outcome)
         (filter (comp some? val))
         (into {}))))

(defn ^:nrepl sayid-get-workspace-data
  "Return the recorded call tree as data - a list of root call nodes, each a map
  whose `children` are more such nodes - for clients that render it themselves.
  The rendered counterpart is `sayid-get-workspace`; see doc/nrepl-api.md for the
  shape."
  [msg]
  (reply:data msg (mapv node->data (:children (sd/ws-deref!)))))

(defn- query-tree->data
  "The data counterpart of `query-tree->trio`: the tree's matched calls as a list
  of `node->data` maps."
  [tree]
  (mapv node->data (:children tree)))

(defn- magic-recusive-eval
  "Lets us send vars to nrepl client and back. Madness."
  [frm]
  (cond (vector? frm) (mapv magic-recusive-eval frm)
        (seq? frm) (eval frm)
        :else frm))

(defn- run-query
  "Parse a printed QUERY form (a string), eval any embedded vars via
  `magic-recusive-eval`, and run it.  Returns the result tree."
  [query]
  (->> query
       read-string
       (map magic-recusive-eval)
       (apply query*)))

(defn ^:nrepl sayid-query
  [{:keys [query] :as msg}]
  ;; TODO default to name-only view for empty query?
  (reply:clj->nrepl msg (sd/with-view (query-tree->trio (run-query query)))))

(defn ^:nrepl sayid-query-data
  "Data counterpart of `sayid-query`: run QUERY and return the matched calls as
  node data.  See doc/nrepl-api.md."
  [{:keys [query] :as msg}]
  (reply:data msg (query-tree->data (run-query query))))

(defn ^:nrepl sayid-query-by-id-data
  "Data counterpart of `sayid-query-by-id`."
  [{:keys [trace-id mod] :as msg}]
  (reply:data msg (query-tree->data
                   (buf-query-tree [:id (keyword trace-id)] mod))))

(defn ^:nrepl sayid-query-by-fn-data
  "Data counterpart of `sayid-query-by-fn`."
  [{:keys [fn-name mod] :as msg}]
  (reply:data msg (query-tree->data
                   (buf-query-tree [#'parent-name-or-name (symbol fn-name)] mod))))

(defn ^:nrepl sayid-query-form-at-point-data
  "Data counterpart of `sayid-query-form-at-point`: the recorded calls matching
  the form at FILE/LINE as node data.  See doc/nrepl-api.md."
  [{:keys [file line] :as msg}]
  (reply:data msg (query-tree->data (sd/ws-query-by-file-pos file line))))

(def sayid-nrepl-ops
  (->> *ns*
       ns-interns
       vals
       (filter #(-> % meta :nrepl))
       (map #(vector (-> % meta :name str) %))
       (into {})))

(defn wrap-sayid
  [handler]
  (fn [{:keys [op] :as msg}]
    (if-let [sayid-op (get sayid-nrepl-ops op)]
      ;; Catch `Throwable' so an error can't leave the op without a terminal
      ;; `:done' and hang the client.  Only Sayid's own ops are guarded here -
      ;; errors from downstream middleware belong to them, not to us.
      (try
        (sayid-op msg)
        (catch Throwable e
          (reply:error msg e)))
      (handler msg))))

(set-descriptor! #'wrap-sayid
                 {:handles (zipmap (keys sayid-nrepl-ops)
                                   (repeat {:doc "A Sayid debugger/profiler operation. See https://github.com/clojure-emacs/sayid for details."
                                            :returns {}
                                            :requires {}}))})
