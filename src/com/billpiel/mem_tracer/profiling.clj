(ns com.billpiel.mem-tracer.profiling
  (require [com.billpiel.mem-tracer.trace :as tr]
           [com.billpiel.mem-tracer.recording :as rec]
           [com.billpiel.mem-tracer.util.other :as util]))

(defn calculate-log-score [])

(defn merge-metric-values
  [a b]
  ((cond (number? a) +
         (set? a) clojure.set/union
         :default (throw (Exception. (format "Cant' merge this: '%s'" a))))
   a b))

(defn merge-fn-metrics
  [& rest]
  (apply merge-with
         #(merge-with merge-metric-values % %2)
         rest))

(defn finalize-metrics
  [fn-ms]
  (util/apply-to-map-vals (fn [metrics]
                            (let [arg-cardinality (-> metrics :arg-set count)
                                  call-count (:count metrics)
                                  gross-time-sum (:gross-time-sum metrics)
                                  repeat-arg-pct (- 1 (/ arg-cardinality
                                                         call-count
                                                         1.0))]
                              (-> metrics
                                  (dissoc :arg-set)
                                  (assoc :gross-time-avg (/ gross-time-sum
                                                             call-count
                                                             1.0)
                                          :net-time-avg  (/ (:net-time-sum metrics)
                                                            call-count
                                                            1.0)
                                          :arg-cardinality arg-cardinality
                                          :repeat-arg-pct repeat-arg-pct
                                          :gross-of-repeats (* gross-time-sum
                                                               repeat-arg-pct)))))
                          fn-ms))

(defn get-fn-metrics
  [tree]
  (let [{{:keys [gross-time net-time arg-set]} :profiling} tree]
    (apply merge-fn-metrics
           {(-> tree :name keyword)
            {:count 1
             :gross-time-sum gross-time
             :net-time-sum net-time
             :arg-set arg-set}}
           (some->> tree
                    :children
                    (map get-fn-metrics)))))

(defn add-durations-to-tree
  [tree]
  (let [gross-time (->> tree
                        ((juxt :ended-at :started-at))
                        (apply util/diff-dates-in-msec))
        children (->> tree
                      :children
                      (mapv add-durations-to-tree))
        kids-time (->> children
                       (map (comp :gross-time :profiling))
                       (apply +))
        net-time (- gross-time kids-time)]
    (assoc tree
           :children children
           :profiling {:gross-time gross-time
                       :net-time net-time
                       :kids-time kids-time
                       :arg-set #{(:args tree)}})))

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
