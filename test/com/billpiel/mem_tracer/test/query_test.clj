(ns com.billpiel.mem-tracer.test.query-test
  (:require [com.billpiel.mem-tracer.query :as q]
            [midje.sweet :refer :all]))

(comment "

            A
          B     C
        D  E     F
          G H     I
             J   K L

")

(def test-trace {:name "A"
                 :depth 0
                 :args [1 2]
                 :return 3
                 :children [{:name "B"
                             :depth 1
                             :args [3 4 5]
                             :return :b-return
                             :children [{:name "D"
                                         :depth 2
                                         :args []
                                         :return 4
                                         :children []}
                                        {:name "E"
                                         :depth 2
                                         :args [:a 1 :b 2]
                                         :return 5
                                         :children []}]}
                            {:name "C"
                             :depth 1
                             :args [1 {:a [10 11 12]} 5]
                             :return 8
                             :children [{:name "F"
                                         :depth 2
                                         :args [2 5 9]
                                         :return "return F"
                                         :children [{:name "I"
                                                     :depth 3
                                                     :args []
                                                     :return 0
                                                     :children [{:name "L"
                                                                 :depth 4
                                                                 :args []
                                                                 :return 0
                                                                 :children []}]}]}]}]})

(def test-zipr (q/trace->zipper test-trace))

(fact "simple"
  (q/q test-zipr [:depth] 1) => [{:name "B"
                                  :depth 1
                                  :args [3 4 5]
                                  :return :b-return
                                  :children []}
                                 {:name "C"
                                  :depth 1
                                  :args [1 {:a [10 11 12]} 5]
                                  :return 8
                                  :children []}])

(fact "segment"
  (q/q test-zipr :s
       [:name] "C"
       [:name] "I")
  => [{:args [1 {:a [10 11 12]} 5],
       :children
       [{:args [2 5 9],
         :children
         [{:args [], :children [], :depth 3, :name "I", :return 0}],
         :depth 2,
         :name "F",
         :return "return F"}],
       :depth 1,
       :name "C",
       :return 8}])

(fact "ancestors"
  (q/q test-zipr :a
       [:name] "B"
       [:name] "I")
  => [{:args [1 2],
       :children
       [{:args [3 4 5],
         :children [],
         :depth 1,
         :name "B",
         :return :b-return}
        {:args [1 {:a [10 11 12]} 5],
         :children
         [{:args [2 5 9],
           :children
           [{:args [], :children [], :depth 3, :name "I", :return 0}],
           :depth 2,
           :name "F",
           :return "return F"}],
         :depth 1,
         :name "C",
         :return 8}],
       :depth 0,
       :name "A",
       :return 3}])
