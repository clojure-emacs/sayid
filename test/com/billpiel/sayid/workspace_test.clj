(ns com.billpiel.sayid.workspace-test
  (:require [clojure.test :as t]
            [com.billpiel.sayid.workspace :as ws]
            [com.billpiel.sayid.test-utils :as t-utils]))

(def ^:dynamic *shelf*)
(def ^:dynamic *ws*)

(defn fixture
  [f]
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (binding [*shelf* '$ws
              *ws* (atom (ws/default-workspace))]
      (f)
      (remove-ns *shelf*))))

(t/use-fixtures :each fixture)

(t/deftest save-as
  (t/testing "ws save-as"
        (t/is (= (ws/deep-deref! (ws/save-as! *ws*
                                              *shelf*
                                              'test1))
                 {:children []
                  :depth 0
                  :id :root10
                  :path [:root10]
                  :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                  :ws-slot '$ws/test1
                  :arg-map nil})))

      (ws/reset-to-nil! *ws*)

      (t/testing "saved successful?"
        (t/is (= (ws/deep-deref! @(ns-resolve (the-ns *shelf*)
                                              'test1))
                 {:children []
                  :depth 0
                  :id :root10
                  :path [:root10]
                  :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                  :ws-slot '$ws/test1
                  :arg-map nil}))))

(t/deftest load-with-symbol
  (t/testing "load with symbol"
    (ws/save-as! *ws* *shelf* 'test1)
    (ws/reset-to-nil! *ws*)

    (ws/load! *ws* *shelf* 'test1)

    (t/is (= (ws/deep-deref! *ws*)
             {:children []
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:ns #{}, :fn #{}, :inner-fn #{}}
              :ws-slot '$ws/test1
              :arg-map nil}))))

(t/deftest load-with-value
  (t/testing "load with value"
    (ws/save-as! *ws* *shelf* 'test1)
    (ws/reset-to-nil! *ws*)

    (t/testing "; start with nil"
      (t/is (= (ws/deep-deref! *ws*)
               nil)))

    (t/testing "; loading"
      (ws/load! *ws* *shelf* @(ns-resolve '$ws 'test1))
      (t/is (= (ws/deep-deref! *ws*)
               {:children []
                :depth 0
                :id :root10
                :path [:root10]
                :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                :ws-slot '$ws/test1
                :arg-map nil})))))

(t/deftest load-with-keyword
  (t/testing "load with keyword"
    (ws/save-as! *ws* *shelf* 'test1)
    (ws/reset-to-nil! *ws*)

    (t/testing "; start with nil"
      (t/is (= (ws/deep-deref! *ws*)
               nil)))

    (t/testing "; loading"
      (ws/load! *ws* *shelf* :test1)

      (t/is (= (ws/deep-deref! *ws*)
               {:children []
                :depth 0
                :id :root10
                :path [:root10]
                :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                :ws-slot '$ws/test1
                :arg-map nil})))))

(t/deftest forced-load
  (t/testing "forced load"
    (ws/save-as! *ws* *shelf* 'test1)
    (ws/reset-to-nil! *ws*)
    (ws/init! *ws*)

    
    (t/is (thrown? Exception
                   (ws/load! *ws* *shelf* 'test1)))

    (t/testing "; didn't overwrite"
      (t/is (= (ws/deep-deref! *ws*)
               {:children []
                :depth 0
                :id :root11
                :path [:root11]
                :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                :ws-slot nil
                :arg-map nil})))

    (t/testing "; loading"
      (ws/load! *ws* *shelf* 'test1 :f)

      (t/is (= (ws/deep-deref! *ws*)
               {:children []
                :depth 0
                :id :root10
                :path [:root10]
                :traced {:ns #{}, :fn #{}, :inner-fn #{}}
                :ws-slot '$ws/test1
                :arg-map nil})))))
