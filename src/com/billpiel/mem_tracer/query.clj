(ns com.billpiel.mem-tracer.query)

(defn entry->seq
  [entry]
  (lazy-cat [entry] (mapcat entry->seq (:children entry))))

(defn query-cat
  [qry-fn entry]
  (->> entry
       entry->seq
       (keep #(when (qry-fn %) %))))
