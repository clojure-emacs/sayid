(ns com.billpiel.sayid.recording-test
  (:require [com.billpiel.sayid.recording :as r]
            [com.billpiel.sayid.workspace :as w]
            [com.billpiel.sayid.test-utils :as t-utils]
            [midje.sweet :refer :all]))

(fact-group
    (with-redefs [gensym (t-utils/mock-gensym-fn)]
      (let [shelf '$rec
            rec (atom (r/mk-recording []))
            ws (w/default-workspace)]

        (fact "save-as"
          @(r/save-as! rec shelf 'test1)
          => {:children []
              :depth 0
              :id :rec10
              :path [:rec10]
              :rec-slot '$rec/test1})

        (reset! rec {})

        (fact "load - not forced, fails"
          (r/load! rec shelf 'test1)
          => (throws Exception))

        (fact "load - forced"
          (r/load! rec shelf 'test1 :f)
          => {:children []
              :depth 0
              :id :rec10
              :path [:rec10]
              :rec-slot '$rec/test1})

        (fact "coerce & load"
          (r/coerce&load! rec ws shelf)
          @rec => {:children []
                   :depth 0
                   :id :rec12
                   :path [:rec12]
                   :rec-slot nil})

        (remove-ns shelf))))
