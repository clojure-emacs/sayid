(ns com.billpiel.sayid.profiling-test
  (:require [com.billpiel.sayid.profiling :as p]
            [com.billpiel.sayid.core :as mt]
            [com.billpiel.sayid.trace :as tr]
            [com.billpiel.sayid.test-utils :as t-utils]
            com.billpiel.sayid.test.ns1
            [midje.sweet :refer :all]))

(fact-group "basic test"
  (tr/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (with-out-str (mt/ws-reset!))
  (mt/rec-reset!)
  (with-redefs [tr/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (mt/ws-add-trace-ns! com.billpiel.sayid.test.ns1)
    (com.billpiel.sayid.test.ns1/func-sleep-10)
    (mt/rec-load-from-ws!)
    (let  [recp (p/assoc-tree-with-profile @mt/recording)]

      (fact "profile function metrics are correct"
        (:profile recp)
        => {:com.billpiel.sayid.test.ns1/func-sleep-10
            {:arg-cardinality 1,
             :count 1,
             :gross-of-repeats 0.0,
             :gross-time-avg 15.0,
             :gross-time-sum 15,
             :net-time-avg 4.0,
             :net-time-sum 4,
             :repeat-arg-pct 0.0},
            :com.billpiel.sayid.test.ns1/func-sleep-20
            {:arg-cardinality 1,
             :count 2,
             :gross-of-repeats 5.0,
             :gross-time-avg 5.0,
             :gross-time-sum 10,
             :net-time-avg 3.0,
             :net-time-sum 6,
             :repeat-arg-pct 0.5},
            :com.billpiel.sayid.test.ns1/func-sleep-30
            {:arg-cardinality 1,
             :count 5,
             :gross-of-repeats 4.0,
             :gross-time-avg 1.0,
             :gross-time-sum 5,
             :net-time-avg 1.0,
             :net-time-sum 5,
             :repeat-arg-pct 0.8}})

      (fact :dev "profile metrics are correct"
            (->> recp
                 :children
                 (clojure.walk/prewalk #(if (-> % meta :com.billpiel.sayid.trace/tree)
                                          (select-keys % [:children :profiling])
                                          %)))
            => [{:children
                 [{:children
                   [{:children []
                     :profiling
                     {:arg-set #{[]} :gross-time 1 :kids-time 0 :net-time 1}}
                    {:children []
                     :profiling
                     {:arg-set #{[]} :gross-time 1 :kids-time 0 :net-time 1}}]
                   :profiling
                   {:arg-set #{[]} :gross-time 5 :kids-time 2 :net-time 3}}
                  {:children []
                   :profiling
                   {:arg-set #{[]} :gross-time 1 :kids-time 0 :net-time 1}}
                  {:children
                   [{:children []
                     :profiling
                     {:arg-set #{[]} :gross-time 1 :kids-time 0 :net-time 1}}
                    {:children []
                     :profiling
                     {:arg-set #{[]} :gross-time 1 :kids-time 0 :net-time 1}}]
                   :profiling
                   {:arg-set #{[]} :gross-time 5 :kids-time 2 :net-time 3}}]
                 :profiling
                 {:arg-set #{[]} :gross-time 15 :kids-time 11 :net-time 4}}]))

    (tr/untrace-ns* 'com.billpiel.sayid.test.ns1)))
