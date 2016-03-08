(ns com.billpiel.sayid.test.query-test
  (:require [com.billpiel.sayid.core :as mm]
            [com.billpiel.sayid.query2 :as q]
            [midje.sweet :refer :all]))

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

(fact "simple"
  (mm/qt test-trace [:depth 1]) =>
  [{:args [3 4 5]
    :children ()
    :depth 1
    :id 2
    :name "B"
    :return :b-return}
   {:args [1 {:a [10 11 12]} 5]
    :children ()
    :depth 1
    :id 3
    :name "C"
    :return 8}])

(fact "range"
  (mapv q/traverse-tree-dissoc-zipper
        (mm/qt test-trace :r
               [:name "C"]
               [:name "I"]))
  => [{:args [1 {:a [10 11 12]} 5]
       :children [{:args [2 5 9]
                   :children [{:args []
                               :children []
                               :depth 3
                               :id 5
                               :name "I"
                               :return 0}]
                   :depth 2
                   :id 4
                   :name "F"
                   :return "return F"}]
       :depth 1
       :id 3
       :name "C"
       :return 8}])

(fact "ancestors"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace :a
                                             [:name "B"]
                                             [:name "I"]))
  => [{:args [1 2]
       :children [{:args [3 4 5]
                   :children []
                   :depth 1
                   :id 2
                   :name "B"
                   :return :b-return}
                  {:args [1 {:a [10 11 12]} 5]
                   :children [{:args [2 5 9]
                               :children [{:args []
                                           :children []
                                           :depth 3
                                           :id 5
                                           :name "I"
                                           :return 0}]
                               :depth 2
                               :id 4
                               :name "F"
                               :return "return F"}]
                   :depth 1
                   :id 3
                   :name "C"
                   :return 8}]
       :depth 0
       :id 1
       :name "A"
       :return 3}])

(fact "descendants"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace :d
                                             [:name "B"]
                                             [:name "I"]))
  => [{:args [3 4 5]
       :children [{:args []
                   :children []
                   :depth 2
                   :id 10
                   :name "D"
                   :return 4}
                  {:args [:a 1 :b 2]
                   :children []
                   :depth 2
                   :id 11
                   :name "E"
                   :return 5}]
       :depth 1
       :id 2
       :name "B"
       :return :b-return}
      {:args []
       :children [{:args []
                   :children []
                   :depth 4
                   :id 6
                   :name "L"
                   :return 0}]
       :depth 3
       :id 5
       :name "I"
       :return 0}])


(fact "descendants - limited distance"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace
                                             :d 1
                                             [:name "A"]
                                             [:name "C"]))
  => [{:args [1 2]
       :children [{:args [3 4 5]
                   :children []
                   :depth 1
                   :id 2
                   :name "B"
                   :return :b-return}
                  {:args [1 {:a [10 11 12]} 5]
                   :children [{:args [2 5 9]
                               :children []
                               :depth 2
                               :id 4
                               :name "F"
                               :return "return F"}]
                   :depth 1
                   :id 3
                   :name "C"
                   :return 8}]
       :depth 0
       :id 1
       :name "A"
       :return 3}])

(fact "ancestors and descendants"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace
                                             :ad
                                             [:name "E"]
                                             [:name "C"]))
  => [{:args [1 2]
       :children [{:args [3 4 5]
                   :children [{:args [:a 1 :b 2]
                               :children []
                               :depth 2
                               :id 11
                               :name "E"
                               :return 5}]
                   :depth 1
                   :id 2
                   :name "B"
                   :return :b-return}
                  {:args [1 {:a [10 11 12]} 5]
                   :children [{:args [2 5 9]
                               :children [{:args []
                                           :children [{:args []
                                                       :children []
                                                       :depth 4
                                                       :id 6
                                                       :name "L"
                                                       :return 0}]
                                           :depth 3
                                           :id 5
                                           :name "I"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 7
                                           :name "M"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 8
                                           :name "N"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 9
                                           :name "O"
                                           :return 0}]
                               :depth 2
                               :id 4
                               :name "F"
                               :return "return F"}]
                   :depth 1
                   :id 3
                   :name "C"
                   :return 8}]
       :depth 0
       :id 1
       :name "A"
       :return 3}])


(fact "ancestors and descendants -- limited distance"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace
                                             :ad 2
                                             [:name "E"]
                                             [:name "C"]))
  => [{:args [1 2]
       :children [{:args [3 4 5]
                   :children [{:args [:a 1 :b 2]
                               :children []
                               :depth 2
                               :id 11
                               :name "E"
                               :return 5}]
                   :depth 1
                   :id 2
                   :name "B"
                   :return :b-return}
                  {:args [1 {:a [10 11 12]} 5]
                   :children [{:args [2 5 9]
                               :children [{:args []
                                           :children []
                                           :depth 3
                                           :id 5
                                           :name "I"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 7
                                           :name "M"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 8
                                           :name "N"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 9
                                           :name "O"
                                           :return 0}]
                               :depth 2
                               :id 4
                               :name "F"
                               :return "return F"}]
                   :depth 1
                   :id 3
                   :name "C"
                   :return 8}]
       :depth 0
       :id 1
       :name "A"
       :return 3}])


(fact "wildcard"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace
                                             :w
                                             [:name "I"]))
  => [{:args [1 2]
       :children [{:args [1 {:a [10 11 12]} 5]
                   :children [{:args [2 5 9]
                               :children [{:args []
                                           :children [{:args []
                                                       :children []
                                                       :depth 4
                                                       :id 6
                                                       :name "L"
                                                       :return 0}]
                                           :depth 3
                                           :id 5
                                           :name "I"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 7
                                           :name "M"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 8
                                           :name "N"
                                           :return 0}
                                          {:args []
                                           :children []
                                           :depth 3
                                           :id 9
                                           :name "O"
                                           :return 0}]
                               :depth 2
                               :id 4
                               :name "F"
                               :return "return F"}]
                   :depth 1
                   :id 3
                   :name "C"
                   :return 8}]
       :depth 0
       :id 1
       :name "A"
       :return 3}])

(fact :dev "wildcard -- limited distance"
      (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace
                                                 :w 1
                                                 [:name "I"]))
      => [{:args [2 5 9]
           :children [{:args []
                       :children [{:args []
                                   :children []
                                   :depth 4
                                   :id 6
                                   :name "L"
                                   :return 0}]
                       :depth 3
                       :id 5
                       :name "I"
                       :return 0} {:args []
                       :children []
                       :depth 3
                       :id 7
                       :name "M"
                       :return 0} {:args []
                       :children []
                       :depth 3
                       :id 8
                       :name "N"
                       :return 0} {:args []
                       :children []
                       :depth 3
                       :id 9
                       :name "O"
                       :return 0}]
           :depth 2
           :id 4
           :name "F"
           :return "return F"}])

(fact "siblings"
  (mapv q/traverse-tree-dissoc-zipper (mm/qt test-trace :s
                                             [:name "E"]))
  => [{:args []
       :children []
       :depth 2
       :id 10
       :name "D"
       :return 4}
      {:args [:a 1 :b 2]
       :children []
       :depth 2
       :id 11
       :name "E"
       :return 5}])
