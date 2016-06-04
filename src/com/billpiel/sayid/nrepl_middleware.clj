(ns com.billpiel.sayid.nrepl-middleware
  (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.string-output :as so]
            [com.billpiel.sayid.trace :as tr]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rts]
            [com.billpiel.sayid.util.other :as util]
            [clojure.tools.namespace.find :as ns-find]))

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
  (println "=============================")
  (println (clj->nrepl out))
  (println "=============================")
  (try (t/send (:transport msg)
               (response-for msg
                             :value (clj->nrepl out)))
       (catch Exception e
         (println "EXCEPTION!")
         (println e)))
  (send-status-done msg))

(defn find-ns-sym
  [file]
  (println file)
  (some->> file
           slurp
           (re-find #"\(ns\s+(.+)\b")
           second
           symbol))

(defn get-form-at-pos-in-source
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
  (meta (get-form-at-pos-in-source file line source)))

(defn parse-ns-name-from-source
  [source]
  (second (re-find #"\(\s*ns\s+([\w$.*-]+)"
                   source)))

(defn tree-contains-inner-trace?
  [tree]
  (->> tree
       (tree-seq map? :children)
       (filter #(contains? % :src-pos))
       first
       nil?
       not))

(defn replay!
  [trees]
  (doseq [{:keys [name args]} trees]
    (apply (resolve name)
           args)))

(defn inner-replay!
  [[{:keys [name]} :as matches]]
  (sd/ws-add-inner-trace-fn!* name)
  (doseq [{:keys [name args]} matches]
    (apply (resolve name)
           args)))

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
      (sd/ws-query* [:id ids])
      [])))

(defn process-line-meta
  [line-meta]
  (mapv (fn [[n m]]
          [n
           (-> m
               (update-in [:path] str)
               (update-in [:header] #(when % 1)))])
        line-meta))

;; ======================


(defn ^:nrepl sayid-get-meta-at-point
  [{:keys [transport source file line] :as msg}]
  (t/send transport
          (response-for msg
                        :value (str (get-meta-at-pos-in-source file line source))))
  (send-status-done msg))

(defn audit-ns->summary-view
  [audit-ns]
  (let [[ns-sym audit-fns] audit-ns
        fn-count (count audit-fns)
        traced-count (->> audit-fns
                          (map second)
                          (map :trace-type)
                          (filter #{:fn :inner-fn})
                          count)]
    [{:ns ns-sym} (format "  %s / %s  %s\n" traced-count fn-count ns-sym)]))

(defn audit-fn->view
  [[ fn-sym {:keys [trace-type trace-selection] :as  fn-meta}]]
  [fn-meta (format "  %s %s %s\n"
                   (case trace-selection
                     :fn "O"
                     :inner-fn "I"
                     :ns " "
                     nil "x"
                     :else "?")
                   (case trace-type
                     :fn "E"
                     :inner-fn "E"
                     nil "D"
                     :else "?")
                   fn-sym)])

(defn audit-fn-group->view
  [[ns-sym audit-fns]]
  (concat [{} (format "- in ns %s\n" ns-sym)]
          (mapcat audit-fn->view audit-fns)))

(defn audit->top-view
  [audit & [ns-sym]]
  (concat [{} "Traced namespaces:\n"]
          (mapcat audit-ns->summary-view (:ns audit))
          [{} "\n\nTraced functions:\n"]
          (mapcat audit-fn-group->view (:fn audit))))

(defn audit->ns-view
  [audit & [ns-sym]]
  (concat [{:ns ns-sym} (format "Namespace %s\n" ns-sym)]
          (mapcat audit-fn->view
                  (-> audit :ns (get ns-sym)))
          [{} "\n\nTraced functions:\n"]
          (mapcat audit-fn->view
                  (-> audit :fn (get ns-sym)))))

(defn ^:nrepl sayid-show-traced
  [{:keys [transport ns] :as msg}]
  (println "*** sayid-show-traced")
  (println ns)
  (let [audit (-> @sd/workspace :traced tr/audit-traces)
        audit-view (if (not (or (nil? ns) (empty? ns)))
                     (audit->ns-view audit (symbol ns))
                     (audit->top-view audit))]
    (->> audit-view
         (partition 2)
         (reply:clj->nrepl msg))))

(defn ^:nrepl sayid-trace-fn
  [{:keys [transport fn-name fn-ns type] :as msg}]
  (println [fn-name fn-ns type])
  (case type
        "outer" (sd/ws-add-trace-fn!* (util/qualify-sym fn-ns fn-name))
        "inner" (sd/ws-add-inner-trace-fn!* (util/qualify-sym fn-ns fn-name)))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-enable
  [{:keys [transport fn-name fn-ns] :as msg}]
  (println "**** sayid-trace-fn-enable")
  (println (util/qualify-sym fn-ns fn-name))
  (sd/ws-enable-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-disable
  [{:keys [transport fn-name fn-ns] :as msg}]
  (println "**** sayid-trace-fn-disable")
  (println (util/qualify-sym fn-ns fn-name))
  (sd/ws-disable-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-trace-fn-remove
  [{:keys [transport fn-name fn-ns] :as msg}]
  (println "**** sayid-trace-fn-remove")
  (println (util/qualify-sym fn-ns fn-name))
  (sd/ws-remove-trace-fn! (util/qualify-sym fn-ns fn-name))
  (send-status-done msg))

(defn ^:nrepl sayid-replay-workspace
  [{:keys [transport] :as msg}]
  (println "sayid-replay-workspace")
  (let [kids (:children (sd/ws-deref!))]
    (sd/ws-cycle-all-traces!)
    (sd/ws-clear-log!)
    (replay! kids))
  (send-status-done msg))

(defn ^:nrepl sayid-replay-at-point
  [{:keys [transport source file line] :as msg}]
  (try (let [{start-line :line} (get-meta-at-pos-in-source file line source)
             matches (query-ws-by-file-line-range file start-line line)
             matches' (when-not (empty? matches)
                        (do (replay! matches)
                            (query-ws-by-file-line-range file start-line line)))

             out (if-not (nil? matches')
                   (-> matches'
                       util/wrap-kids
                       so/tree->string+meta)
                   {:string (str "No trace records found for function at line: " line)})]
         (t/send transport (response-for msg
                                         :value (:string out)
                                         :meta (-> out :meta process-line-meta))))
       (catch Exception e
         (t/send transport (response-for msg
                                         :value (with-out-str (clojure.stacktrace/print-stack-trace e)))))
       (finally
         (send-status-done msg))))

(defn ^:nrepl sayid-replay-with-inner-trace-at-point
  [{:keys [transport source file line] :as msg}]
  (try (let [{start-line :line} (get-meta-at-pos-in-source file line source)
             matches (query-ws-by-file-line-range file start-line line)
             [{:keys [name inner-path]}] matches
             kids (:children (sd/ws-deref!))
             _ (do (when-not (empty? matches)
                     (sd/ws-cycle-all-traces!)
                     (sd/ws-clear-log!)
                     (when (nil? inner-path) ;; don't inner-trace an inner trace
                       (sd/ws-add-inner-trace-fn!* name))
                     (replay! kids)))
             matches' (query-ws-by-file-line-range file start-line line)
             out (if-not (or (empty? matches)
                             (empty? matches'))
                   (-> matches'
                       util/wrap-kids
                       so/tree->meta-string-pairs
                       clj->nrepl)
                   (clj->nrepl [[nil (str "No trace records found for function at line: " line)]]))]
         (t/send transport (response-for msg
                                         :value out)))
       (catch Exception e
         (t/send transport (response-for msg
                                         :value (with-out-str (clojure.stacktrace/print-stack-trace e)))))
       (finally
         (send-status-done msg))))

(defn ^:nrepl sayid-replay-with-inner-trace
  [{:keys [transport func] :as msg}]
  (try
    (let [fn-sym (symbol func)
          kids (:children (sd/ws-deref!))
          _ (do (sd/ws-add-inner-trace-fn!* fn-sym)
                (sd/ws-cycle-all-traces!)
                (sd/ws-clear-log!)
                (replay! kids))
          matches (-> (sd/ws-query* [:parent-name fn-sym])
                      util/wrap-kids
                      so/tree->meta-string-pairs
                      clj->nrepl)
          out (clj->nrepl matches)]
      (t/send transport (response-for msg
                                      :value out)))
    (catch Exception e
      (println e)
      (println (with-out-str ;; I don't know why this is necessary
                 (clojure.stacktrace/print-stack-trace e)))
      (t/send transport (response-for msg
                                      :value (with-out-str (clojure.stacktrace/print-stack-trace e)))))
    (finally
      (send-status-done msg))))

(defn ^:nrepl sayid-query-form-at-point
  [{:keys [file line] :as msg}]
  (reply:clj->nrepl msg
                    (-> (sd/ws-query-by-file-pos file line)
                        util/wrap-kids
                        so/tree->text-prop-pair)))

(defn sayid-buf-query
  [q-vec mod-str]
  (let [[_ sk sn] (re-find #"(\w+)\s*(\d+)?" mod-str)
        k (keyword sk)
        n (util/->int sn)
        query (remove nil? [k n q-vec])]
    (-> (apply sd/ws-query* query)
        util/wrap-kids
        so/tree->text-prop-pair)))

(defn ^:nrepl sayid-buf-query-id-w-mod
  [{:keys [trace-id mod] :as msg}]
  (reply:clj->nrepl msg
                    (sayid-buf-query [:id (keyword trace-id)]
                                     mod)))

(defn ^:nrepl sayid-buf-query-fn-w-mod
  [{:keys [fn-name mod] :as msg}]
  (reply:clj->nrepl msg (sayid-buf-query [(some-fn :parent-name :name)
                              (symbol fn-name)]
                             mod)))

;; this func is unfortunate
(defn str-vec->arg-path
  [[kw idx]]
  (let [kw' (keyword kw)]
    (cond (nil? idx) [kw']
          (string? idx) [kw' (symbol idx)]
          :else [kw' idx])))

(defn ^:nrepl sayid-buf-def-at-point
  [{:keys [transport trace-id path] :as msg}]
  (let [path' (str-vec->arg-path path)]
    (util/def-ns-var '$s '* (-> [:id (keyword trace-id)] ;;TODO use intern
                                sd/ws-query*
                                first
                                (get-in path'))))
  (t/send transport (response-for msg :value "Def'd as $s/*"))
  (send-status-done msg))

(defn ^:nrepl sayid-buf-pprint-at-point
  [{:keys [transport trace-id path] :as msg}]
  (let [path' (str-vec->arg-path path)
        value (-> [:id (keyword trace-id)] ;;TODO use intern
                  sd/ws-query*
                  first
                  (get-in path'))]
    (t/send transport (response-for msg :value (clj->nrepl [[nil (so/pprint-str value)]]))))
  (send-status-done msg))

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
  (sd/ws-disable-all-traces!)
  (doall (map sd/ws-add-trace-ns!*
              (ns-find/find-namespaces-in-dir (java.io.File. dir))))
  (sd/ws-cycle-all-traces!)
  (send-status-done msg))

(defn ^:nrepl sayid-trace-ns-in-file
  [{:keys [transport file] :as msg}]
  (println "sayid-trace-ns-in-file")
  (println file)
  (->> file
       find-ns-sym
       (#(do (println %) %))
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

(defn ^:nrepl sayid-set-printer
  [{:keys [transport printer] :as msg}]
  (println printer)
  (if (.startsWith printer ".")
    (reset! sd/printer sd/default-printer)
    (->> (str "[" printer "]")
         read-string
         (apply sd/set-printer!)))
  (send-status-done msg))

(defn ^:nrepl sayid-get-workspace
  [msg]
  (reply:clj->nrepl msg
                    (sd/with-this-printer [:children]
                      (so/tree->text-prop-pair (sd/ws-deref!)))))

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
    ((get sayid-nrepl-ops op handler) msg)))

(set-descriptor! #'wrap-sayid
                 {:handles (zipmap (keys sayid-nrepl-ops)
                                   (repeat {:doc "docs?"
                                            :returns {}
                                            :requires {}}))})
