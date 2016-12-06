(ns com.billpiel.sayid.core-test
  (:require [clojure.test :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.trace :as sdt]
            [com.billpiel.sayid.query2 :as sdq]
            [com.billpiel.sayid.test-utils :as t-utils]
            [com.billpiel.sayid.test-ns1 :as ns1]
            [com.billpiel.sayid.string-output2 :as sds]))

(t/deftest basic-workflow
  (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (let [trace (sd/ws-deref!)
          expected-trace {:arg-map nil,
                          :children
                          [{:args [:a],
                            :path [:root10 :11],
                            :children
                            [{:args [:a],
                              :path [:root10 :11 :12],
                              :children [],
                              :meta
                              {:arglists '([arg1]),
                               :column 1,
                               :file "FILE",
                               :line 4,
                               :name 'func2,
                               :ns (the-ns 'com.billpiel.sayid.test-ns1)},
                              :return :a,
                              :started-at 1
                              :name 'com.billpiel.sayid.test-ns1/func2,
                              :arg-map {'arg1 :a},
                              :id :12,
                              :ended-at 2
                              :depth 2}],
                            :meta
                            {:arglists '([arg1]),
                             :column 1,
                             :file "FILE",
                             :line 8,
                             :name 'func1,
                             :ns (the-ns 'com.billpiel.sayid.test-ns1)},
                            :return :a,
                            :started-at 0
                            :name 'com.billpiel.sayid.test-ns1/func1,
                            :arg-map {'arg1 :a},
                            :id :11,
                            :ended-at 3
                            :depth 1}],
                          :depth 0,
                          :id :root10,
                          :path [:root10],
                          :traced
                          {:inner-fn #{}, :fn #{}, :ns #{'com.billpiel.sayid.test-ns1}},
                          :ws-slot nil}]

      (t/testing "log is correct"
        (t/is (= (-> trace
                     ((t-utils/redact-file-fn [:children 0 :meta :file]
                                              [:children 0 :children 0 :meta :file])))
                 expected-trace)))


      (t/testing "remove trace"
        (sd/ws-remove-trace-ns! 'ns1)
        (ns1/func1 :b)
        (t/is (= (-> (sd/ws-deref!)
                     ((t-utils/redact-file-fn [:children 0 :meta :file]
                                              [:children 0 :children 0 :meta :file])))
                 (assoc expected-trace
                        :traced {:fn #{}, :ns #{}, :inner-fn #{}})))))

    (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)))
