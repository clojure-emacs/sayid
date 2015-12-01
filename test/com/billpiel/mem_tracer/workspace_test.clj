(ns com.billpiel.mem-tracer.workspace-test
  (:require [com.billpiel.mem-tracer.workspace :as w]
            [com.billpiel.mem-tracer.test-utils :as t-utils]
            [midje.sweet :refer :all]))

(fact "save-as"
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (let [shelf '$ws
          ws (atom (w/default-workspace))]

      (w/deref-workspace! (w/save-as! ws shelf 'test1))
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}
          :ws-slot '$ws/test1}

      (w/deref-workspace! @(ns-resolve (the-ns shelf)
                                       'test1))
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}
          :ws-slot '$ws/test1}

      (remove-ns shelf))))
