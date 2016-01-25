(ns com.billpiel.mem-tracer.profiling
  (require [com.billpiel.mem-tracer.trace :as tr]
           [com.billpiel.mem-tracer.recording :as rec]
           [com.billpiel.mem-tracer.util.other :as util]))

(defn calculate-log-score [])

(defn merge-fn-metrics
  [& rest]
  (apply merge-with
         #(apply merge-with + % %2)
         rest))

(defn finalize-metrics
  [fn-ms]
  (util/apply-to-map-vals #(merge %
                                  {:gross-time-avg (/ (:gross-time-sum %)
                                                      (:count %)
                                                      1.0)
                                   :net-time-avg  (/ (:net-time-sum %)
                                                     (:count %)
                                                     1.0)})
                          fn-ms))

(defn get-fn-metrics
  [tree]
  (let [{{:keys [gross-time net-time]} :profiling} tree]
    (apply merge-fn-metrics
           {(-> tree :name keyword)
            {:count 1
             :gross-time-sum gross-time
             :net-time-sum net-time}}
           (map get-fn-metrics* (:children tree)))
    (merge-with + {:a 2} nil)))

(defn get-fn-metrics-finalized
  [tree]
  (-> tree
      get-fn-metrics
      finalize-metrics))

(defn add-durations-to-tree
  [tree]
  (let [gross-time (->> tree
                      ((juxt :ended-at :started-at))
                      (apply util/diff-dates-in-sec))
        children (->> tree
                      :children
                      (mapv add-durations-to-tree))
        kids-time (->> children
                           (map :duration)
                           (apply +))
        net-time (- gross-time kids-time)]
    (merge tree
           {:children children
            :profiling {:gross-time gross-time
                        :net-time net-time
                        :kids-time kids-time}})))

(defn add-metrics-to-rec
  [rec]
  (let [rec' (->> rec
                  :children
                  (mapv add-durations-to-tree)
                  rec/mk-recording)]
    (->> rec'
        :children
        (map get-fn-metrics)
        (apply merge-fn-metrics)
        finalize-metrics
        (assoc rec' :fn-metrics))))

(defn get-report
  [rec])

(defn print-report
  [rec])
