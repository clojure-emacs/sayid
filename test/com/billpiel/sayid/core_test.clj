(ns com.billpiel.sayid.core-test
  (:require [clojure.test :as t]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.trace :as sdt]
            [com.billpiel.sayid.query2 :as sdq]
            [com.billpiel.sayid.test-utils :as t-utils]
            [com.billpiel.sayid.test-ns1 :as ns1]
            [com.billpiel.sayid.string-output2 :as sds]))


(defn- fixture
  [f]
  (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)))

(t/use-fixtures :each fixture)

(t/deftest version-test
  (t/is (string? sd/version)))

(t/deftest add-remove-trace-ns

  (t/testing "ws-add-trace-ns!"
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
      (t/is (= (-> trace
                   ((t-utils/redact-file-fn [:children 0 :meta :file]
                                            [:children 0 :children 0 :meta :file])))
               expected-trace))

      (t/testing "ws-remove-trace-ns!"
        (sd/ws-remove-trace-ns! 'ns1)
        (ns1/func1 :b)
        (t/is (= (-> (sd/ws-deref!)
                     ((t-utils/redact-file-fn [:children 0 :meta :file]
                                              [:children 0 :children 0 :meta :file])))
                 (assoc expected-trace
                        :traced {:fn #{}, :ns #{}, :inner-fn #{}})))))))


(t/deftest ed-all-traces

  (sd/ws-add-trace-ns! com.billpiel.sayid.test-ns1)

  (t/testing "ws-disable-all-traces!"
    (sd/ws-disable-all-traces!)
    (com.billpiel.sayid.test-ns1/func1 :a)
    (t/is (= (sd/ws-deref!)
             {:children []
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:inner-fn #{}, :fn #{}, :ns #{'com.billpiel.sayid.test-ns1}}
              :ws-slot nil
              :arg-map nil})))

  (t/testing "ws-enable-all-traces!"
    (sd/ws-enable-all-traces!)
    (com.billpiel.sayid.test-ns1/func1 :a)

    (t/is (= (-> (sd/ws-deref!)
                 ((t-utils/redact-file-fn [:children 0 :meta :file]
                                          [:children 0 :children 0 :meta :file])))
             {:arg-map nil
              :children [{:arg-map {'arg1 :a}
                          :args [:a]
                          :children [{:arg-map {'arg1 :a}
                                      :args [:a]
                                      :children []
                                      :depth 2
                                      :ended-at 2
                                      :id :12
                                      :meta {:arglists '([arg1])
                                             :column 1
                                             :file "FILE"
                                             :line 4
                                             :name 'func2
                                             :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                                      :name 'com.billpiel.sayid.test-ns1/func2
                                      :path [:root10 :11 :12]
                                      :return :a
                                      :started-at 1}]
                          :depth 1
                          :ended-at 3
                          :id :11
                          :meta {:arglists '([arg1])
                                 :column 1
                                 :file "FILE"
                                 :line 8
                                 :name 'func1
                                 :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                          :name 'com.billpiel.sayid.test-ns1/func1
                          :path [:root10 :11]
                          :return :a
                          :started-at 0}]
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:inner-fn #{}
                       :fn #{}
                       :ns #{'com.billpiel.sayid.test-ns1}}
              :ws-slot nil}))))

(t/deftest remove-all-traces
  (sd/ws-add-trace-ns! ns1)

  (t/testing "remove-all-traces! works"
    (with-out-str (sd/ws-remove-all-traces!))
    (ns1/func1 :a)

    (t/is (= (sd/ws-deref!)
             {:children []
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:fn #{} :ns #{} :inner-fn #{}}
              :ws-slot nil
              :arg-map nil}))))

(t/deftest exception-thrown
  (t/testing "exception thrown"
    (let [trace-root (sd/ws-add-trace-ns! com.billpiel.sayid.test-ns1)
          _ (try
              (com.billpiel.sayid.test-ns1/func-throws :a)
              (catch Throwable t))
          trace (sd/ws-deref!)]

      (t/testing "; trace root"
        (t/is (= (dissoc trace :children)
                 {:depth 0
                  :id :root10
                  :path [:root10]
                  :traced {:fn #{}, :ns #{'com.billpiel.sayid.test-ns1}, :inner-fn #{}}
                  :ws-slot nil
                  :arg-map nil})))

      (t/testing "; children count"
        (t/is (= (-> trace :children count)
                 1)))

      (t/testing "; throw cause"
        (t/is (= (-> trace :children first :throw :cause)
                 "Exception from func-throws: :a")))

      (t/testing "; first child"
        (t/is (= (-> trace
                     :children
                     first
                     (dissoc :throw)
                     ((t-utils/redact-file-fn [:meta :file])))
                 {:args [:a]
                  :children []
                  :depth 1
                  :ended-at 1
                  :id :11
                  :name 'com.billpiel.sayid.test-ns1/func-throws
                  :path [:root10 :11]
                  :started-at 0
                  :meta {:arglists '([arg1])
                         :column 1
                         :file "FILE"
                         :line 12
                         :name 'func-throws
                         :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                  :arg-map {'arg1 :a}}))))))

(t/deftest querying
  (t/testing "q macro"
    (let [trace-root (sd/ws-add-trace-ns! com.billpiel.sayid.test-ns1)
          _ (com.billpiel.sayid.test-ns1/func3-1 3 8)
          trace (sd/ws-deref!)]

      (t/testing "; find node by name and all parents"
        (t/is (=
               (->> (sd/w-q :a [:name #".*func3-4"])
                    sdq/traverse-tree-dissoc-zipper
                    ((t-utils/redact-file-fn [:children 0 :meta :file]
                                             [:children 0 :children 0 :meta :file]
                                             [:children 0 :children 0 :children 0 :meta :file])))
               {:arg-map nil
                :children [{:arg-map {'arg1 3
                                      'arg2 8}
                            :args [3 8]
                            :children [{:arg-map {'arg1 8}
                                        :args [8]
                                        :children [{:arg-map {'arg1 8}
                                                    :args [8]
                                                    :children []
                                                    :depth 3
                                                    :ended-at 7
                                                    :id :15
                                                    :meta {:arglists '([arg1])
                                                           :column 1
                                                           :file "FILE"
                                                           :line 16
                                                           :name 'func3-4
                                                           :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                                                    :name 'com.billpiel.sayid.test-ns1/func3-4
                                                    :path [:root10 :11 :13 :15]
                                                    :return 8
                                                    :started-at 6}]
                                        :depth 2
                                        :ended-at 8
                                        :id :13
                                        :meta {:arglists '([arg1])
                                               :column 1
                                               :file "FILE"
                                               :line 28
                                               :name 'func3-3
                                               :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                                        :name 'com.billpiel.sayid.test-ns1/func3-3
                                        :path [:root10 :11 :13]
                                        :return 8
                                        :started-at 3}]
                            :depth 1
                            :ended-at 11
                            :id :11
                            :meta {:arglists '([arg1 arg2])
                                   :column 1
                                   :file "FILE"
                                   :line 33
                                   :name 'func3-1
                                   :ns (the-ns 'com.billpiel.sayid.test-ns1)}
                            :name 'com.billpiel.sayid.test-ns1/func3-1
                            :path [:root10 :11]
                            :return 13
                            :started-at 0}]
                :depth 0
                :id :root10
                :path [:root10]
                :traced {:inner-fn #{}
                         :fn #{}
                         :ns #{'com.billpiel.sayid.test-ns1}}
                :ws-slot nil}))))))
