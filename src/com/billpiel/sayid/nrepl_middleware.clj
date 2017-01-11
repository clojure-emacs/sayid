(ns com.billpiel.sayid.nrepl-middleware
  (:require [clojure.stacktrace :as st]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.string-output2 :as so]
            [com.billpiel.sayid.view :as v]
            [com.billpiel.sayid.trace :as tr]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rts]
            [com.billpiel.sayid.util.other :as util]
            [clojure.tools.namespace.find :as ns-find]
            [com.billpiel.sayid.util.find-ns :as find-ns]
            [tamarin.core :as tam]))


(def views (atom {}))
(def selected-view (atom nil))


(defn try-find-ns-root
  [ns-sym]
  (let [depth (some-> ns-sym str (clojure.string/split #"\.") count)]
    (util/$- some-> ns-sym ns-interns vals first meta :file (clojure.string/split #"/") (drop-last depth $) (clojure.string/join "/" $))))

(defn find-all-ns-roots
  []
  (some->> (all-ns) (map str) (map symbol) (map try-find-ns-root) distinct (remove empty?)))


(defn register-view!
  [name view]
  (swap! views
         assoc
         name
         view))

(defn query*
  [& args]
  (if args
    (assoc (apply sd/ws-query*
                  args)
           ::query-args
           args)
    (assoc (sd/ws-view!)
           ::query-args
           nil)))

(defn query-tree->trio
  [tree]
  (conj (vec (so/tree->text-prop-pair tree))
        (-> tree ::query-args pr-str)))

(defn clj->nrepl*
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

(defn send-status-done
  [msg]
  (t/send (:transport msg)
          (response-for msg :status :done)))

(defn reply:clj->nrepl
  [msg out]
  (try (t/send (:transport msg)
               (response-for msg
                             :value (clj->nrepl out)))
       (catch Exception e
         (println "EXCEPTION!")
         (println e)))
  (send-status-done msg))

(defn find-ns-sym
  [file]
  (some->> file
           slurp
           (re-find #"\(ns\s+(.+)\b")
           second
           symbol))

(defn get-top-form-at-pos-in-source
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

(defn get-meta-at-pos-in-source
  [file line source]
  (meta (get-top-form-at-pos-in-source file line source)))

(defn pos-inside-line-column?
  [pos-line pos-column start-line end-line start-col end-col]
  (if (= start-line pos-line end-line)
    (<= start-col pos-column end-col)
    (or (and (= start-line pos-line)
             (<= start-col pos-column))
        (and (= end-line pos-line)
             (< pos-column end-col))
        (< start-line pos-line end-line))))

(defn get-sym-at-pos-in-source
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

(defn parse-ns-name-from-source ;; TODO don't use this
  [source]
  (second (re-find #"\(\s*ns\s+([\w$.*-]+)"
                   source)))

(defn ^:nrepl sayid-trace-fn-enable-at-point
  [{:keys [transport file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym
        (util/resolve-to-qual-sym ns-sym sym)]
    (when qual-sym
      (sd/ws-enable-trace-fn! qual-sym))
    (reply:clj->nrepl msg qual-sym)))


(defn ^:nrepl sayid-trace-fn-disable-at-point
  [{:keys [transport file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym
        (util/resolve-to-qual-sym ns-sym sym)]
    (when qual-sym
      (sd/ws-disable-trace-fn! qual-sym))
    (reply:clj->nrepl msg qual-sym)))

(defn ^:nrepl sayid-trace-fn-outer-trace-at-point
  [{:keys [transport file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym
        (util/resolve-to-qual-sym ns-sym sym)]
    (when qual-sym
      (sd/ws-add-trace-fn!* qual-sym))
    (reply:clj->nrepl msg qual-sym)))

(defn ^:nrepl sayid-trace-fn-inner-trace-at-point
  [{:keys [transport file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym
        (util/resolve-to-qual-sym ns-sym sym)]
    (when qual-sym
      (sd/ws-add-inner-trace-fn!* qual-sym))
    (reply:clj->nrepl msg qual-sym)))

(defn ^:nrepl sayid-remove-trace-fn-at-point
  [{:keys [transport file line column source] :as msg}]
  (let [sym (get-sym-at-pos-in-source file line column source)
        ns-sym (symbol (parse-ns-name-from-source source))
        qual-sym
        (util/resolve-to-qual-sym ns-sym sym)]
    (when qual-sym
      (sd/ws-remove-trace-fn! qual-sym))
    (reply:clj->nrepl msg qual-sym)))

(defn tree-contains-inner-trace?
  [tree]
  (->> tree
       (tree-seq map? :children)
       (filter #(contains? % :src-pos))
       first
       nil?
       not))

(defn query-ws-by-file-line-range
  "Find the tree node that: is smallest or starts latest, contains line
  and starts at or after start-line."
  [file start-line line]
  (let [ws (sd/ws-deref!)
        ids (q/get-ids-from-file-line-range ws
                                            file
                                            (or start-line line)
                                            line)]
    (if-not (empty? ids)
      (query* [:id ids])
      nil)))

(defn process-line-meta
  [line-meta]
  (mapv (fn [[n m]]
          [n
           (-> m
               (update-in [:path] str)
               (update-in [:header] #(when % 1)))])
        line-meta))

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
  [{:keys [transport ns] :as msg}]
  (let [audit (-> @sd/workspace :traced tr/audit-traces)
        audit-view (if (not (or (nil? ns) (empty? ns)))
                     (so/audit->ns-view audit (symbol ns))
                     (so/audit->top-view audit))]
    (->> audit-view
         so/tokens->text-prop-pair
         (reply:clj->nrepl msg))))

(defn count-traces
  [trace-audit]
  (+ (count  (for [v1 (-> trace-audit :ns vals)
                   v2 (vals v1)]
               v2))
     (count  (for [v1 (-> trace-audit :fn vals)
                   v2 (vals v1)]
               v2))))

(defn count-enabled-traces
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

(defn ^:nrepl sayid-trace-fn
  [{:keys [transport fn-name fn-ns type] :as msg}]
  (case type
        "outer" (sd/ws-add-trace-fn!* (util/qualify-sym fn-ns fn-name))
        "inner" (sd/ws-add-inner-trace-fn!* (util/qualify-sym fn-ns fn-name)))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-enable
  [{:keys [transport fn-name fn-ns] :as msg}]
  (sd/ws-enable-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-disable
  [{:keys [transport fn-name fn-ns] :as msg}]
  (sd/ws-disable-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-remove
  [{:keys [transport fn-name fn-ns] :as msg}]
  (sd/ws-remove-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-enable
  [{:keys [transport fn-ns] :as msg}]
  (sd/ws-enable-trace-ns! (symbol fn-ns))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-disable
  [{:keys [transport fn-ns] :as msg}]
  (sd/ws-disable-trace-ns! (symbol fn-ns))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-remove
  [{:keys [transport fn-ns] :as msg}]
  (sd/ws-remove-trace-ns! (symbol fn-ns))
  (send-status-done msg))

(defn ^:nrepl sayid-query-form-at-point
  [{:keys [file line] :as msg}]
  (reply:clj->nrepl msg
                    (-> (sd/ws-query-by-file-pos file line)
                        so/tree->text-prop-pair)))

(defn sayid-buf-query
  [q-vec mod-str]
  (let [[_ sk sn] (re-find #"(\w+)\s*(\d+)?" mod-str)
        k (keyword sk)
        n (util/->int sn)
        query (remove nil? [k n q-vec])]
    (sd/with-view (->> query
                       (apply query*)
                       query-tree->trio))))

(defn ^:nrepl sayid-buf-query-id-w-mod
  [{:keys [trace-id mod] :as msg}]
  (reply:clj->nrepl msg
                    (sayid-buf-query [:id (keyword trace-id)]
                                     mod)))

(def parent-name-or-name (some-fn :parent-name :name))

(defn ^:nrepl sayid-buf-query-fn-w-mod
  [{:keys [fn-name mod] :as msg}]
  (reply:clj->nrepl msg (sayid-buf-query [#'parent-name-or-name
                                          (symbol fn-name)]
                                         mod)))

;; this func is unfortunate
(defn str-vec->arg-path
  [[kw & idx]]
  (let [kw' (keyword kw)
        str->sym (fn [s] (if (string? s)
                           (symbol s)
                           s))]
    (into [kw'] (mapv str->sym idx))))

;; ===== gen-instance-expr helpers

(defn find-arg-list-by-length
  [n [first-list & rest-lists]]
  (let [cfl (count first-list)]
    (cond (nil? first-list) nil
          
          (or (= n cfl)
              (and (-> first-list reverse rest first (= '&))
                   (>= n (dec cfl))))
          first-list

          :else (recur n rest-lists))))

(defn get-args-sym-template
  [arglist]
  (let [convert-non-syms (fn [v] (if (symbol? v)
                                   v
                                   '*))]
    (util/$- ->> arglist
             (remove #{'&})
             (map convert-non-syms)
             (concat $ (repeat (last $))))))

(defn find-available-sym
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

(defn lazy-find-available-sym
  [prefix-seq init-taken]
  (let [next (find-available-sym nil (first prefix-seq) init-taken)]
    (lazy-cat [next]
              (lazy-find-available-sym (rest prefix-seq)
                                       (conj init-taken next)))))


(defn mk-avail-sym-lazy-seq
  [n arglists]
  (lazy-find-available-sym (get-args-sym-template (find-arg-list-by-length n
                                                                           arglists))
                           (-> '$s
                               create-ns
                               ns-interns
                               keys
                               (or []))))

;; END ===== gen-instance-expr helpers

(defn gen-instance-expr
  [tree]
  (let [arg-count (-> tree :args count)
        arglists (-> tree :meta :arglists)
        arglist-template-seq (mk-avail-sym-lazy-seq arg-count
                                                    arglists)]
    (doseq [pair (map vector
                      arglist-template-seq
                      (:args tree))]
      (apply util/def-ns-var
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

(defn ^:nrepl sayid-buf-def-at-point
  [{:keys [transport trace-id path] :as msg}]
  (let [path' (str-vec->arg-path path)]
    (util/def-ns-var '$s '* (-> [:id (keyword trace-id)]
                                sd/ws-query*
                                :children
                                first
                                (get-in path'))))
  (t/send transport (response-for msg :value "Def'd as $s/*"))
  (send-status-done msg))

(defn ^:nrepl sayid-buf-pprint-at-point
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

(defn ^:nrepl sayid-trace-all-ns-in-dir
  [{:keys [transport dir] :as msg}]
  (doall (map sd/ws-add-trace-ns!*
              (ns-find/find-namespaces-in-dir (java.io.File. dir))))
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

(defn ^:nrepl sayid-remove-all-traces
  [{:keys [transport] :as msg}]
  (sd/ws-remove-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-disable-all-traces
  [{:keys [transport] :as msg}]
  (sd/ws-disable-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-enable-all-traces
  [{:keys [transport] :as msg}]
  (sd/ws-enable-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-get-workspace
  [msg]
  (reply:clj->nrepl msg
                    (sd/with-this-view (or @sd/view
                                      (v/mk-simple-view {}))
                      (query-tree->trio (sd/ws-view!)))))

(defn magic-recusive-eval
  "Lets us send vars to nrepl client and back. Madness."
  [frm]
  (cond (vector? frm) (mapv magic-recusive-eval frm)
        (seq? frm) (eval frm)
        :else frm))

(defn ^:nrepl sayid-query
  [{:keys [transport query] :as msg}]
  ;; TODO default to name-only view for empty query?
  (sd/with-view (->> query
                     read-string
                     (map magic-recusive-eval)
                     (apply query*)
                     query-tree->trio
                     (reply:clj->nrepl msg))))

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
    (try
      ((get sayid-nrepl-ops op handler) msg)
      (catch Throwable e
          (println (.getMessage e))
          (st/print-stack-trace e)
          (reply:clj->nrepl msg (.getMessage e))))))


(set-descriptor! #'wrap-sayid
                 {:handles (zipmap (keys sayid-nrepl-ops)
                                   (repeat {:doc "docs?"
                                            :returns {}
                                            :requires {}}))})
