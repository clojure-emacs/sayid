(ns com.billpiel.sayid.recording-test
  (:require [clojure.test :as t]
            [com.billpiel.sayid.recording :as r]
            [com.billpiel.sayid.workspace :as w]
            [com.billpiel.sayid.test-utils :as t-utils]))

(t/deftest recording
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (let [shelf '$rec
          rec (atom (r/mk-recording []))
          ws (w/default-workspace)]

      (t/testing "save-as"
        (t/is (= @(r/save-as! rec shelf 'test1)
                 {:children []
                  :depth 0
                  :id :rec10
                  :path [:rec10]
                  :rec-slot '$rec/test1})))

      (reset! rec {})

      (t/testing "load - not forced, fails"
        (t/is (thrown? Exception
                       (r/load! rec shelf 'test1))))

      (t/testing "load - forced"
        (t/is (= (r/load! rec shelf 'test1 :f)
                 {:children []
                  :depth 0
                  :id :rec10
                  :path [:rec10]
                  :rec-slot '$rec/test1})))

      (t/testing "coerce & load"
        (r/coerce&load! rec ws shelf)
        (t/is (= @rec
                 {:children []
                  :depth 0
                  :id :rec12
                  :path [:rec12]
                  :rec-slot nil})))

      (remove-ns shelf))))
