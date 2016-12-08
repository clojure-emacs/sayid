(ns com.billpiel.sayid.query-test
  (:require [clojure.test :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.test-utils :as t-utils]))

(comment "

            A
          B     C
        D  E     F
          G H     I  M N O
             J    L

")

(def test-trace {:id 1
                 :name "A"
                 :depth 0
                 :args [1 2]
                 :return 3
                 :children [{:id 2
                             :name "B"
                             :depth 1
                             :args [3 4 5]
                             :return :b-return
                             :children [{:id 10
                                         :name "D"
                                         :depth 2
                                         :args []
                                         :return 4
                                         :children []}
                                        {:id 11
                                         :name "E"
                                         :depth 2
                                         :args [:a 1 :b 2]
                                         :return 5
                                         :children []}]}
                            {:id 3
                             :name "C"
                             :depth 1
                             :args [1 {:a [10 11 12]} 5]
                             :return 8
                             :children [{:id 4
                                         :name "F"
                                         :depth 2
                                         :args [2 5 9]
                                         :return "return F"
                                         :children [{:id 5
                                                     :name "I"
                                                     :depth 3
                                                     :args []
                                                     :return 0
                                                     :children [{:id 6
                                                                 :name "L"
                                                                 :depth 4
                                                                 :args []
                                                                 :return 0
                                                                 :children []}]}
                                                    {:id 7
                                                     :name "M"
                                                     :depth 3
                                                     :args []
                                                     :return 0
                                                     :children []}
                                                    {:id 8
                                                     :name "N"
                                                     :depth 3
                                                     :args []
                                                     :return 0
                                                     :children []}
                                                    {:id 9
                                                     :name "O"
                                                     :depth 3
                                                     :args []
                                                     :return 0
                                                     :children []}]}]}]})

(defn- fixture
  [f]
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (f)))

(t/use-fixtures :each fixture)

(t/deftest simple
  (t/is (= (sd/tree-query test-trace [:depth 1])
           {:id :query-result10,
            :children
            [{:id 2,
              :name "B",
              :depth 1,
              :args [3 4 5],
              :return :b-return,
              :children ()}
             {:id 3,
              :name "C",
              :depth 1,
              :args [1 {:a [10 11 12]} 5],
              :return 8,
              :children ()}]})))

(t/deftest range
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace :r
                           [:name "C"]
                           [:name "I"]))
           {:id :query-result10,
            :children
            [{:id 3,
              :name "C",
              :depth 1,
              :args [1 {:a [10 11 12]} 5],
              :return 8,
              :children
              [{:id 4,
                :name "F",
                :depth 2,
                :args [2 5 9],
                :return "return F",
                :children
                [{:id 5,
                  :name "I",
                  :depth 3,
                  :args [],
                  :return 0,
                  :children []}]}]}]})))

(t/deftest ancestors
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace :a
                           [:name "B"]
                           [:name "I"]))
           {:id :query-result10,
            :children
            [{:id 1,
              :name "A",
              :depth 0,
              :args [1 2],
              :return 3,
              :children
              [{:id 2,
                :name "B",
                :depth 1,
                :args [3 4 5],
                :return :b-return,
                :children []}
               {:id 3,
                :name "C",
                :depth 1,
                :args [1 {:a [10 11 12]} 5],
                :return 8,
                :children
                [{:id 4,
                  :name "F",
                  :depth 2,
                  :args [2 5 9],
                  :return "return F",
                  :children
                  [{:id 5,
                    :name "I",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}]}]}]}]})))


(t/deftest descendants
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace :d
                           [:name "B"]
                           [:name "I"]))
           {:id :query-result10,
            :children
            [{:id 2,
              :name "B",
              :depth 1,
              :args [3 4 5],
              :return :b-return,
              :children
              [{:id 10, :name "D", :depth 2, :args [], :return 4, :children []}
               {:id 11,
                :name "E",
                :depth 2,
                :args [:a 1 :b 2],
                :return 5,
                :children []}]}
             {:id 5,
              :name "I",
              :depth 3,
              :args [],
              :return 0,
              :children
              [{:id 6,
                :name "L",
                :depth 4,
                :args [],
                :return 0,
                :children []}]}]})))

(t/deftest descendants-limited-distance
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace
                           :d 1
                           [:name "A"]
                           [:name "C"]))
           {:id :query-result10,
            :children
            [{:id 1,
              :name "A",
              :depth 0,
              :args [1 2],
              :return 3,
              :children
              [{:id 2,
                :name "B",
                :depth 1,
                :args [3 4 5],
                :return :b-return,
                :children []}
               {:id 3,
                :name "C",
                :depth 1,
                :args [1 {:a [10 11 12]} 5],
                :return 8,
                :children
                [{:id 4,
                  :name "F",
                  :depth 2,
                  :args [2 5 9],
                  :return "return F",
                  :children []}]}]}]})))

(t/deftest ancestors-and-descendants
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace
                           :ad
                           [:name "E"]
                           [:name "C"]))
           {:id :query-result10,
            :children
            [{:id 1,
              :name "A",
              :depth 0,
              :args [1 2],
              :return 3,
              :children
              [{:id 2,
                :name "B",
                :depth 1,
                :args [3 4 5],
                :return :b-return,
                :children
                [{:id 11,
                  :name "E",
                  :depth 2,
                  :args [:a 1 :b 2],
                  :return 5,
                  :children []}]}
               {:id 3,
                :name "C",
                :depth 1,
                :args [1 {:a [10 11 12]} 5],
                :return 8,
                :children
                [{:id 4,
                  :name "F",
                  :depth 2,
                  :args [2 5 9],
                  :return "return F",
                  :children
                  [{:id 5,
                    :name "I",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children
                    [{:id 6,
                      :name "L",
                      :depth 4,
                      :args [],
                      :return 0,
                      :children []}]}
                   {:id 7,
                    :name "M",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 8,
                    :name "N",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 9,
                    :name "O",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}]}]}]}]})))

(t/deftest ancestors-and-descendants-limited-distance
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace
                           :ad 2
                           [:name "E"]
                           [:name "C"]))
           {:id :query-result10,
            :children
            [{:id 1,
              :name "A",
              :depth 0,
              :args [1 2],
              :return 3,
              :children
              [{:id 2,
                :name "B",
                :depth 1,
                :args [3 4 5],
                :return :b-return,
                :children
                [{:id 11,
                  :name "E",
                  :depth 2,
                  :args [:a 1 :b 2],
                  :return 5,
                  :children []}]}
               {:id 3,
                :name "C",
                :depth 1,
                :args [1 {:a [10 11 12]} 5],
                :return 8,
                :children
                [{:id 4,
                  :name "F",
                  :depth 2,
                  :args [2 5 9],
                  :return "return F",
                  :children
                  [{:id 5,
                    :name "I",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 7,
                    :name "M",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 8,
                    :name "N",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 9,
                    :name "O",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}]}]}]}]})))

(t/deftest wildcard
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace
                           :w
                           [:name "I"]))
           {:id :query-result10,
            :children
            [{:id 1,
              :name "A",
              :depth 0,
              :args [1 2],
              :return 3,
              :children
              [{:id 3,
                :name "C",
                :depth 1,
                :args [1 {:a [10 11 12]} 5],
                :return 8,
                :children
                [{:id 4,
                  :name "F",
                  :depth 2,
                  :args [2 5 9],
                  :return "return F",
                  :children
                  [{:id 5,
                    :name "I",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children
                    [{:id 6,
                      :name "L",
                      :depth 4,
                      :args [],
                      :return 0,
                      :children []}]}
                   {:id 7,
                    :name "M",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 8,
                    :name "N",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}
                   {:id 9,
                    :name "O",
                    :depth 3,
                    :args [],
                    :return 0,
                    :children []}]}]}]}]})))

(t/deftest wildcard-limited-distance
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace
                           :w 1
                           [:name "I"]))
           {:id :query-result10,
            :children
            [{:id 4,
              :name "F",
              :depth 2,
              :args [2 5 9],
              :return "return F",
              :children
              [{:id 5,
                :name "I",
                :depth 3,
                :args [],
                :return 0,
                :children
                [{:id 6,
                  :name "L",
                  :depth 4,
                  :args [],
                  :return 0,
                  :children []}]}
               {:id 7, :name "M", :depth 3, :args [], :return 0, :children []}
               {:id 8, :name "N", :depth 3, :args [], :return 0, :children []}
               {:id 9,
                :name "O",
                :depth 3,
                :args [],
                :return 0,
                :children []}]}]})))

(t/deftest siblings
  (t/is (= (q/traverse-tree-dissoc-zipper
            (sd/tree-query test-trace :s
                           [:name "E"]))
           {:id :query-result10,
            :children
            [{:id 10, :name "D", :depth 2, :args [], :return 4, :children []}
             {:id 11,
              :name "E",
              :depth 2,
              :args [:a 1 :b 2],
              :return 5,
              :children []}]})))
