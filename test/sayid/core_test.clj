(ns sayid.core-test
  (:require [clojure.test :as t]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.query :as sdq]
            [sayid.test-utils :as t-utils]
            [sayid.test-ns1 :as ns1]
            [sayid.string-output :as sds]))


(defn- fixture
  [f]
  (sdt/untrace-ns* 'sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (sdt/untrace-ns* 'sayid.test-ns1)))

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
                               :ns (the-ns 'sayid.test-ns1)},
                              :return :a,
                              :started-at 1
                              :name 'sayid.test-ns1/func2,
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
                             :ns (the-ns 'sayid.test-ns1)},
                            :return :a,
                            :started-at 0
                            :name 'sayid.test-ns1/func1,
                            :arg-map {'arg1 :a},
                            :id :11,
                            :ended-at 3
                            :depth 1}],
                          :depth 0,
                          :id :root10,
                          :path [:root10],
                          :traced
                          {:inner-fn #{}, :fn #{}, :ns #{'sayid.test-ns1}},
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

  (sd/ws-add-trace-ns! sayid.test-ns1)

  (t/testing "ws-disable-all-traces!"
    (sd/ws-disable-all-traces!)
    (sayid.test-ns1/func1 :a)
    (t/is (= (sd/ws-deref!)
             {:children []
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:inner-fn #{}, :fn #{}, :ns #{'sayid.test-ns1}}
              :ws-slot nil
              :arg-map nil})))

  (t/testing "ws-enable-all-traces!"
    (sd/ws-enable-all-traces!)
    (sayid.test-ns1/func1 :a)

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
                                             :ns (the-ns 'sayid.test-ns1)}
                                      :name 'sayid.test-ns1/func2
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
                                 :ns (the-ns 'sayid.test-ns1)}
                          :name 'sayid.test-ns1/func1
                          :path [:root10 :11]
                          :return :a
                          :started-at 0}]
              :depth 0
              :id :root10
              :path [:root10]
              :traced {:inner-fn #{}
                       :fn #{}
                       :ns #{'sayid.test-ns1}}
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
    (let [trace-root (sd/ws-add-trace-ns! sayid.test-ns1)
          _ (try
              (sayid.test-ns1/func-throws :a)
              (catch Throwable t))
          trace (sd/ws-deref!)]

      (t/testing "; trace root"
        (t/is (= (dissoc trace :children)
                 {:depth 0
                  :id :root10
                  :path [:root10]
                  :traced {:fn #{}, :ns #{'sayid.test-ns1}, :inner-fn #{}}
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
                  :name 'sayid.test-ns1/func-throws
                  :path [:root10 :11]
                  :started-at 0
                  :meta {:arglists '([arg1])
                         :column 1
                         :file "FILE"
                         :line 12
                         :name 'func-throws
                         :ns (the-ns 'sayid.test-ns1)}
                  :arg-map {'arg1 :a}}))))))

(t/deftest querying
  (t/testing "q macro"
    (let [trace-root (sd/ws-add-trace-ns! sayid.test-ns1)
          _ (sayid.test-ns1/func3-1 3 8)
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
                                                           :ns (the-ns 'sayid.test-ns1)}
                                                    :name 'sayid.test-ns1/func3-4
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
                                               :ns (the-ns 'sayid.test-ns1)}
                                        :name 'sayid.test-ns1/func3-3
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
                                   :ns (the-ns 'sayid.test-ns1)}
                            :name 'sayid.test-ns1/func3-1
                            :path [:root10 :11]
                            :return 13
                            :started-at 0}]
                :depth 0
                :id :root10
                :path [:root10]
                :traced {:inner-fn #{}
                         :fn #{}
                         :ns #{'sayid.test-ns1}}
                :ws-slot nil}))))))

(t/deftest inner-trace-letfn
  ;; Regression test for #14: inner-tracing a function that uses `letfn` used
  ;; to throw "Syntax error compiling fn*", because the fn bindings of the
  ;; expanded `letfn*' form were wrapped with tracing forms.
  (t/testing "inner-tracing a letfn-using fn"
    (sd/ws-add-inner-trace-fn! sayid.test-ns1/func-letfn)
    (t/testing "; doesn't throw and returns the right value"
      (t/is (= 8 (sayid.test-ns1/func-letfn 3))))
    (t/testing "; records the call"
      (t/is (= 1 (-> (sd/ws-deref!) :children count))))))

(t/deftest record-limit-caps-recorded-root-calls
  (t/testing "the recording stops growing at *record-limit* roots"
    ;; Suppress the one-time \"recording is full\" warning to *err*.
    (binding [sdt/*record-limit* 3
              *err* (java.io.StringWriter.)]
      (sd/ws-add-trace-ns! ns1)
      (dotimes [_ 10] (ns1/func1 :a))
      (t/testing "; calls past the cap still run and return normally"
        (t/is (= :b (ns1/func1 :b))))
      (t/testing "; only *record-limit* root calls are recorded"
        (t/is (= 3 (-> (sd/ws-deref!) :children count)))))))

(t/deftest max-trace-depth-drops-nested-calls
  (t/testing "*max-trace-depth* stops recording below the limit"
    (binding [sdt/*max-trace-depth* 1]
      (sd/ws-add-trace-ns! ns1)
      (t/testing "; the call still runs normally"
        (t/is (= :a (ns1/func1 :a))))
      (let [roots (:children (sd/ws-deref!))]
        (t/testing "; the root call is recorded"
          (t/is (= 1 (count roots))))
        (t/testing "; but its nested call (func2) is dropped"
          (t/is (= 0 (-> roots first :children count))))))))

(t/deftest sample-rate-records-one-in-n-root-calls
  (t/testing "*sample-rate* records only one in every N top-level calls"
    (binding [sdt/*sample-rate* 2]
      (sd/ws-add-trace-ns! ns1)
      (dotimes [_ 4] (ns1/func1 :a))
      (t/is (= 2 (-> (sd/ws-deref!) :children count))))))

(t/deftest per-fn-limit-caps-calls-of-each-function
  (t/testing "*per-fn-limit* records at most N calls of a given function"
    ;; Suppress the one-time per-fn cap warning to *err*.
    (binding [sdt/*per-fn-limit* 2
              *err* (java.io.StringWriter.)]
      (sd/ws-add-trace-ns! ns1)
      (dotimes [_ 5] (ns1/func1 :a))
      (t/is (= 2 (-> (sd/ws-deref!) :children count))
            "only *per-fn-limit* calls of func1 are recorded, the rest run untraced"))))

(t/deftest suppressed-root-with-inner-trace-is-skipped-cleanly
  (t/testing "a skipped root that calls an inner-traced fn records nothing, cleanly"
    ;; Regression guard for the suppression fix: without it, the skipped root's
    ;; nested calls leaked in as roots and inner tracing hit a nil parent.
    (binding [sdt/*sample-rate* 1000]
      (sd/ws-add-trace-fn! sayid.test-ns1/func1)
      (sd/ws-add-inner-trace-fn! sayid.test-ns1/func2)
      (t/testing "; the call still runs and returns normally"
        (t/is (= :a (ns1/func1 :a))))
      (t/testing "; and nothing is recorded"
        (t/is (= 0 (-> (sd/ws-deref!) :children count)))))))
