(ns com.billpiel.mem-tracer.profiling-test
  (:require [com.billpiel.mem-tracer.profiling :as p]
            [com.billpiel.mem-tracer.core :as mt]
            [com.billpiel.mem-tracer.trace :as tr]
            [com.billpiel.mem-tracer.test-utils :as t-utils]
            com.billpiel.mem-tracer.test.ns1
            [midje.sweet :refer :all]))

(def mock-now-fn #(t-utils/make-mock-series-lazy-fn identity
                                                    (map (fn [s] (java.util.Date. 101 0 0 0 0 s))
                                                         (range))))

(fact-group "basic test"
  (tr/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/ws-reset!)
  (mt/rec-reset!)
  (with-redefs [tr/now (mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (mt/ws-add-trace-ns! com.billpiel.mem-tracer.test.ns1)
    (com.billpiel.mem-tracer.test.ns1/func-sleep-10)
    (let  [rec  (mt/rec-load-from-ws!)
           recp (p/add-metrics-to-rec rec)]

      (fact "profile function metrics are correct"
        (:fn-metrics recp)
        => {:com.billpiel.mem-tracer.test.ns1/func-sleep-10 {:count 1
                                                             :gross-time-avg 15000.0
                                                             :gross-time-sum 15000
                                                             :net-time-avg 4000.0
                                                             :net-time-sum 4000}
            :com.billpiel.mem-tracer.test.ns1/func-sleep-20 {:count 2
                                                             :gross-time-avg 5000.0
                                                             :gross-time-sum 10000
                                                             :net-time-avg 3000.0
                                                             :net-time-sum 6000}
            :com.billpiel.mem-tracer.test.ns1/func-sleep-30 {:count 5
                                                             :gross-time-avg 1000.0
                                                             :gross-time-sum 5000
                                                             :net-time-avg 1000.0
                                                             :net-time-sum 5000}})

      (fact "profile metrics are correct"
        (->> recp
             :children
             (clojure.walk/prewalk #(if (-> % meta :com.billpiel.mem-tracer.trace/tree)
                                      (select-keys % [:children :profiling])
                                      %)))
        => [{:children [{:children [{:children []
                                     :profiling {:gross-time 1000
                                                 :kids-time 0
                                                 :net-time 1000}} {:children []
                                     :profiling {:gross-time 1000
                                                 :kids-time 0
                                                 :net-time 1000}}]
                         :profiling {:gross-time 5000
                                     :kids-time 2000
                                     :net-time 3000}} {:children []
                         :profiling {:gross-time 1000
                                     :kids-time 0
                                     :net-time 1000}} {:children [{:children []
                                                                   :profiling {:gross-time 1000
                                                                               :kids-time 0
                                                                               :net-time 1000}} {:children []
                                                                   :profiling {:gross-time 1000
                                                                               :kids-time 0
                                                                               :net-time 1000}}]
                         :profiling {:gross-time 5000
                                     :kids-time 2000
                                     :net-time 3000}}]
             :profiling {:gross-time 15000
                         :kids-time 11000
                         :net-time 4000}}]))

    (tr/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)))
