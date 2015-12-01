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


      (w/reset-workspace! ws)

      (w/deref-workspace! @(ns-resolve (the-ns shelf)
                                       'test1))
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}
          :ws-slot '$ws/test1}

      (remove-ns shelf))))

(fact "load"
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (let [shelf '$ws
          ws (atom (w/default-workspace))]

      (w/save-as! ws shelf 'test1)
      (w/reset-workspace! ws)

      (w/load! ws shelf 'test1)

      (w/deref-workspace! ws)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}
          :ws-slot '$ws/test1}

      (remove-ns shelf))))

(fact "forced load"
  (with-redefs [gensym (t-utils/mock-gensym-fn)]
    (let [shelf '$ws
          ws (atom (w/default-workspace))]

      (w/save-as! ws shelf 'test1)
      (w/reset-workspace! ws)
      (w/init-workspace! ws)

      (w/load! ws shelf 'test1)
      => (throws Exception)

      (w/deref-workspace! ws)
      => {:children []
          :depth 0
          :id :root11
          :path [:root11]
          :traced #{}
          :ws-slot nil}

      (w/load! ws shelf 'test1 :f)

      (w/deref-workspace! ws)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}
          :ws-slot '$ws/test1}

      (remove-ns shelf))))
