(ns com.billpiel.sayid.nrepl-middleware
  (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.string-output :as so]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rts]
            [com.billpiel.sayid.util.other :as util]
            [clojure.tools.namespace.find :as ns-find]))

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
  (sd/ws-add-deep-trace-fn!* name)
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
                                            start-line
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

;; ======================


(defn sayid-get-meta-at-point
  [{:keys [transport source file line] :as msg}]
  (t/send transport
          (response-for msg
                        :value (str (get-meta-at-pos-in-source file line source))))
  (send-status-done msg))

(defn sayid-show-traced
  [{:keys [transport] :as msg}]
  (t/send transport (response-for msg :value (clj->nrepl (sd/ws-show-traced*))))
  (send-status-done msg))

(defn sayid-replay-workspace
  [{:keys [transport] :as msg}]
  (println "sayid-replay-workspace")
  (let [kids (:children (sd/ws-deref!))]
    (sd/ws-cycle-all-traces!)
    (sd/ws-clear-log!)
    (replay! kids))
  (send-status-done msg))

(defn sayid-replay-at-point
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

(defn sayid-replay-with-inner-trace
  [{:keys [transport source file line] :as msg}]
  (try (let [{start-line :line} (get-meta-at-pos-in-source file line source)
             matches (query-ws-by-file-line-range file start-line line)
             [{:keys [name]}] matches
             kids (:children (sd/ws-deref!))
             _ (do (sd/ws-cycle-all-traces!)
                   (sd/ws-clear-log!)
                   (sd/ws-add-deep-trace-fn!* name)
                   (replay! kids))
             matches' (query-ws-by-file-line-range file start-line line)
             out (if-not (nil? matches')
                   (-> matches'
                       util/wrap-kids
                       so/tree->meta-string-pairs
                       clj->nrepl)
                   {:string (str "No trace records found for function at line: " line)})]
         (t/send transport (response-for msg
                                         :value out)))
       (catch Exception e
         (t/send transport (response-for msg
                                         :value (with-out-str (clojure.stacktrace/print-stack-trace e)))))
       (finally
         (send-status-done msg))))

(defn sayid-query-form-at-point
  [{:keys [file line] :as msg}]
  (reply:clj->nrepl msg
                    (-> (sd/ws-query-by-file-pos file line)
                        util/wrap-kids
                        so/tree->meta-string-pairs)))

(defn sayid-buf-query
  [q-vec mod-str]
  (let [[_ sk sn] (re-find #"(\w+)\s*(\d+)?" mod-str)
        k (keyword sk)
        n (util/->int sn)
        query (remove nil? [k n q-vec])]
    (-> (apply sd/ws-query* query)
        util/wrap-kids
        so/tree->meta-string-pairs)))

(defn sayid-buf-query-id-w-mod
  [{:keys [trace-id mod] :as msg}]
  (reply:clj->nrepl msg
                    (sayid-buf-query [:id (keyword trace-id)]
                                     mod)))

(defn sayid-buf-query-fn-w-mod
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

(defn sayid-buf-def-at-point
  [{:keys [transport trace-id path] :as msg}]
  (println trace-id)
  (println path)
  (println (type path))
  (let [path' (str-vec->arg-path path)]
    (println path')
    (util/def-ns-var '$s '* (-> [:id (keyword trace-id)] ;;TODO use intern
                                sd/ws-query*
                                first
                                (#(do (println %) %))
                                (get-in path'))))
  (t/send transport (response-for msg :value "Def'd as $s/*"))
  (send-status-done msg))

(defn sayid-clear-log
  [{:keys [transport] :as msg}]
  (sd/ws-clear-log!)
  (send-status-done msg))

(defn sayid-reset-workspace
  [{:keys [transport] :as msg}]
  (sd/ws-reset!)
  (send-status-done msg))

(defn sayid-trace-all-ns-in-dir
  [{:keys [transport dir] :as msg}]
  (doall (map sd/ws-add-trace-ns!*
              (ns-find/find-namespaces-in-dir (java.io.File. dir))))
  (send-status-done msg))

(defn sayid-trace-ns-in-file
  [{:keys [transport file] :as msg}]
  (println "sayid-trace-ns-in-file")
  (println file)
  (->> file
       find-ns-sym
       (#(do (println %) %))
       sd/ws-add-trace-ns!*)
  (send-status-done msg))

(defn sayid-remove-all-traces
  [{:keys [transport] :as msg}]
  (sd/ws-remove-all-traces!)
  (send-status-done msg))

(defn sayid-disable-all-traces
  [{:keys [transport] :as msg}]
  (sd/ws-disable-all-traces!)
  (send-status-done msg))

(defn sayid-set-printer
  [{:keys [transport printer] :as msg}]
  (println printer)
  (if (.startsWith printer ".")
    (reset! sd/printer sd/default-printer)
    (->> (str "[" printer "]")
         read-string
         (apply sd/set-printer!)))
  (send-status-done msg))

(defn sayid-get-workspace
  [msg]
  (reply:clj->nrepl msg
                    (sd/with-this-printer [:children]
                      (so/tree->meta-string-pairs (sd/ws-deref!)))))

(def sayid-nrepl-ops
  {"sayid-query-form-at-point" #'sayid-query-form-at-point
   "sayid-replay-with-inner-trace" #'sayid-replay-with-inner-trace
   "sayid-get-workspace" #'sayid-get-workspace
   "sayid-clear-log" #'sayid-clear-log
   "sayid-reset-workspace" #'sayid-reset-workspace
   "sayid-trace-all-ns-in-dir" #'sayid-trace-all-ns-in-dir
   "sayid-trace-ns-in-file" #'sayid-trace-ns-in-file
   "sayid-remove-all-traces" #'sayid-remove-all-traces
   "sayid-disable-all-traces" #'sayid-disable-all-traces
   "sayid-buf-query-id-w-mod" #'sayid-buf-query-id-w-mod
   "sayid-buf-query-fn-w-mod" #'sayid-buf-query-fn-w-mod
   "sayid-buf-def-at-point" #'sayid-buf-def-at-point
   "sayid-set-printer" #'sayid-set-printer
   "sayid-show-traced" #'sayid-show-traced
   "sayid-replay-at-point" #'sayid-replay-at-point
   "sayid-replay-workspace" #'sayid-replay-workspace
   "sayid-get-meta-at-point" #'sayid-get-meta-at-point})


(defn wrap-sayid
  [handler]
  (fn [{:keys [op] :as msg}]
    ((get sayid-nrepl-ops op handler) msg)))

(set-descriptor! #'wrap-sayid
                 {:handles (zipmap (keys sayid-nrepl-ops)
                                   (repeat {:doc "docs?"
                                            :returns {}
                                            :requires {}}))})

#_ (
    "
TODO
x eval-last-exp -- with trace ns in dir
- eval-last-exp -- with trace all proj ns
x query for form at point
- query for form at point -- with modifiers
x force replay for inner form query
- indicate funcs that were re-ran
x include meta in string responses
x jump to func/form from sayid buffer
- def captured value from sayid buffer



"




     )
