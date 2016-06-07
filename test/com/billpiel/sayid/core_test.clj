(ns com.billpiel.sayid.core-test
  (:require [midje.sweet :refer :all]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.trace :as sdt]
            [com.billpiel.sayid.query2 :as sdq]
            [com.billpiel.sayid.test-utils :as t-utils]
            com.billpiel.sayid.test.ns1
            [com.billpiel.sayid.test.ns1 :as ns1]
            [com.billpiel.sayid.string-output :as so]))


 (fact-group "basic test"
   (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
   (with-out-str (sd/ws-reset!))
   (with-redefs [sdt/now (t-utils/mock-now-fn)
                 gensym (t-utils/mock-gensym-fn)]
     (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)
     (com.billpiel.sayid.test.ns1/func1 :a)
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
                                   :ns (the-ns 'com.billpiel.sayid.test.ns1)},
                                  :return :a,
                                  :started-at 1
                                  :name 'com.billpiel.sayid.test.ns1/func2,
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
                                 :ns (the-ns 'com.billpiel.sayid.test.ns1)},
                                :return :a,
                                :started-at 0
                                :name 'com.billpiel.sayid.test.ns1/func1,
                                :arg-map {'arg1 :a},
                                :id :11,
                                :ended-at 3
                                :depth 1}],
                              :depth 0,
                              :id :root10,
                              :path [:root10],
                              :traced
                              {:inner-fn #{}, :fn #{}, :ns #{'com.billpiel.sayid.test.ns1}},
                              :ws-slot nil}]

          (fact "log is correct"
                    (-> trace
                        ((t-utils/redact-file-fn [:children 0 :meta :file]
                                                 [:children 0 :children 0 :meta :file])))
                    => expected-trace)


          (fact "remove trace"
                  (sd/ws-remove-trace-ns! 'com.billpiel.sayid.test.ns1)
                  (com.billpiel.sayid.test.ns1/func1 :b)
                  (-> (sd/ws-deref!)
                      ((t-utils/redact-file-fn [:children 0 :meta :file]
                                               [:children 0 :children 0 :meta :file])))
                  => (assoc expected-trace
                            :traced {:fn #{}, :ns #{}, :inner-fn #{}})))

     (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)))

 (fact-group "about enable/disable -all-traces!"
  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]

    (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)

    (fact "ws-disable-all-traces! works"
      (sd/ws-disable-all-traces!)
      (com.billpiel.sayid.test.ns1/func1 :a)

      (sd/ws-deref!)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced {:inner-fn #{}, :fn #{}, :ns #{'com.billpiel.sayid.test.ns1}}
          :ws-slot nil
                    :arg-map nil})

    (fact "ws-enable-all-traces! works"
      (sd/ws-enable-all-traces!)
      (com.billpiel.sayid.test.ns1/func1 :a)

      (-> (sd/ws-deref!)
          ((t-utils/redact-file-fn [:children 0 :meta :file]
                                   [:children 0 :children 0 :meta :file])))
      => {:arg-map nil
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
                                         :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                  :name 'com.billpiel.sayid.test.ns1/func2
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
                             :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                      :name 'com.billpiel.sayid.test.ns1/func1
                      :path [:root10 :11]
                      :return :a
                      :started-at 0}]
          :depth 0
                    :id :root10
          :path [:root10]
          :traced {:inner-fn #{}
                   :fn #{}
                   :ns #{'com.billpiel.sayid.test.ns1}}
          :ws-slot nil})

    (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)))

(fact-group "about remove-all-traces!"
  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]

    (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)

    (fact "remove-all-traces! works"
      (with-out-str (sd/ws-remove-all-traces!))
      (com.billpiel.sayid.test.ns1/func1 :a)

      (sd/ws-deref!)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced {:fn #{} :ns #{} :inner-fn #{}}
          :ws-slot nil
                    :arg-map nil})

    (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)))

(def mock-log {:children
               [{:args [:a]
                 :children []
                 :started-at #inst "2010-01-01T01:00:00.000-00:00"
                 :name 'com.billpiel.sayid.test.ns1/func-throws
                 :id :11
                 :parent-id :root10
                 :ended-at 1
                 :throw
                 {:cause "Exception from func-throws: :a"
                  :via
                  [{:type java.lang.Exception
                    :at
                    {:method-name "invoke"
                     :file-name "ns1.clj"
                     :class-name "com.billpiel.sayid.test.ns1$func_throws"
                     :line-number 14}
                    :message "Exception from func-throws: :a"}]
                  :trace
                  [{:method-name "invoke"
                    :file-name "ns1.clj"
                    :class-name "com.billpiel.sayid.test.ns1$func_throws"
                    :line-number 14}
                   {:method-name "applyToHelper"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 154}
                   {:method-name "applyTo"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 144}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$apply"
                    :line-number 624}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name
                    "com.billpiel.sayid.core$trace_fn_call$fn__22284"
                    :line-number 85}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "com.billpiel.sayid.core$trace_fn_call"
                    :line-number 83}
                   {:method-name "doInvoke"
                    :file-name "core.clj"
                    :class-name
                    "com.billpiel.sayid.core$trace_var_STAR_$fn__22290$tracing_wrapper__22291"
                    :line-number 120}
                   {:method-name "invoke"
                    :file-name "RestFn.java"
                    :class-name "clojure.lang.RestFn"
                    :line-number 408}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.sayid.core_test$eval22994$fn__22995$fn__22996$fn__22997$fn__22998"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.sayid.core_test$eval22994$fn__22995$fn__22996$fn__22997"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$with_redefs_fn"
                    :line-number 6861}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.sayid.core_test$eval22994$fn__22995$fn__22996"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "thread_safe_var_nesting.clj"
                    :class-name
                    "midje.util.thread_safe_var_nesting$with_altered_roots_STAR_"
                    :line-number 32}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.sayid.core_test$eval22994$fn__22995"
                    :line-number 1}
                   {:method-name "applyToHelper"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 152}
                   {:method-name "applyTo"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 144}
                   {:method-name "doInvoke"
                    :file-name "AFunction.java"
                    :class-name "clojure.lang.AFunction$1"
                    :line-number 29}
                   {:method-name "invoke"
                    :file-name "RestFn.java"
                    :class-name "clojure.lang.RestFn"
                    :line-number 397}
                   {:method-name "invoke"
                    :file-name "facts.clj"
                    :class-name "midje.checking.facts$check_one$fn__13965"
                    :line-number 31}
                   {:method-name "invoke"
                    :file-name "facts.clj"
                    :class-name "midje.checking.facts$check_one"
                    :line-number 30}
                   {:method-name "invoke"
                    :file-name "facts.clj"
                    :class-name "midje.checking.facts$creation_time_check"
                    :line-number 35}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name "com.billpiel.sayid.core_test$eval22994"
                    :line-number 1}
                   {:method-name "eval"
                    :file-name "Compiler.java"
                    :class-name "clojure.lang.Compiler"
                    :line-number 6703}
                   {:method-name "eval"
                    :file-name "Compiler.java"
                    :class-name "clojure.lang.Compiler"
                    :line-number 6666}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$eval"
                    :line-number 2927}
                   {:method-name "invoke"
                    :file-name "main.clj"
                    :class-name "clojure.main$repl$read_eval_print__6625$fn__6628"
                    :line-number 239}
                   {:method-name "invoke"
                    :file-name "main.clj"
                    :class-name "clojure.main$repl$read_eval_print__6625"
                    :line-number 239}
                   {:method-name "invoke"
                    :file-name "main.clj"
                    :class-name "clojure.main$repl$fn__6634"
                    :line-number 257}
                   {:method-name "doInvoke"
                    :file-name "main.clj"
                    :class-name "clojure.main$repl"
                    :line-number 257}
                   {:method-name "invoke"
                    :file-name "RestFn.java"
                    :class-name "clojure.lang.RestFn"
                    :line-number 1523}
                   {:method-name "invoke"
                    :file-name "interruptible_eval.clj"
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__6509"
                    :line-number 72}
                   {:method-name "applyToHelper"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 152}
                   {:method-name "applyTo"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 144}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$apply"
                    :line-number 624}
                   {:method-name "doInvoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$with_bindings_STAR_"
                    :line-number 1862}
                   {:method-name "invoke"
                    :file-name "RestFn.java"
                    :class-name "clojure.lang.RestFn"
                    :line-number 425}
                   {:method-name "invoke"
                    :file-name "interruptible_eval.clj"
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$evaluate"
                    :line-number 56}
                   {:method-name "invoke"
                    :file-name "interruptible_eval.clj"
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__6551$fn__6554"
                    :line-number 191}
                   {:method-name "invoke"
                    :file-name "interruptible_eval.clj"
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__6546"
                    :line-number 159}
                   {:method-name "run"
                    :file-name "AFn.java"
                    :class-name "clojure.lang.AFn"
                    :line-number 22}
                   {:method-name "runWorker"
                    :file-name "ThreadPoolExecutor.java"
                    :class-name "java.util.concurrent.ThreadPoolExecutor"
                    :line-number 1145}
                   {:method-name "run"
                    :file-name "ThreadPoolExecutor.java"
                    :class-name "java.util.concurrent.ThreadPoolExecutor$Worker"
                    :line-number 615}
                   {:method-name "run"
                    :file-name "Thread.java"
                    :class-name "java.lang.Thread"
                    :line-number 745}]}
                 :depth 1}]
               :depth 0
               :id :root10})

(fact-group "exception thrown"

  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (let [trace-root (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)
          _ (try
              (com.billpiel.sayid.test.ns1/func-throws :a)
              (catch Throwable t))
          trace (sd/ws-deref!)]

      (fact "log is correct"
        (dissoc trace :children)
        => {:depth 0
            :id :root10
            :path [:root10]
            :traced {:fn #{}, :ns #{'com.billpiel.sayid.test.ns1}, :inner-fn #{}}
            :ws-slot nil
                        :arg-map nil})

      (fact "log is correct"
        (-> trace :children count)
        => 1)

      (fact "log is correct"
        (-> trace :children first :throw :cause)
        => "Exception from func-throws: :a")

      (fact "log is correct"
        (-> trace :children first :throw :via first)
        => {:at {:class-name "com.billpiel.sayid.test.ns1$func_throws"
                 :file-name "ns1.clj"
                 :line-number 14
                 :method-name "invoke"}
            :message "Exception from func-throws: :a"
            :type java.lang.Exception})

      (fact "log is correct"
        (-> trace
            :children
            first
            (dissoc :throw)
            ((t-utils/redact-file-fn [:meta :file])))
        => {:args [:a]
            :children []
            :depth 1
            :ended-at 1
            :id :11
            :name 'com.billpiel.sayid.test.ns1/func-throws
            :path [:root10 :11]
            :started-at 0
            :meta {:arglists '([arg1])
                   :column 1
                   :file "FILE"
                   :line 12
                   :name 'func-throws
                   :ns (the-ns 'com.billpiel.sayid.test.ns1)}
            :arg-map {'arg1 :a}})

      (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1))))

(fact-group "querying with query/query"

  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (sd/ws-clear-log!)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (let [trace-root (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)
          _ (com.billpiel.sayid.test.ns1/func3-1 3 8)
          trace (sd/ws-deref!)]

      (fact "find node by name and all parents"
        (def r  (->> (sd/t-query trace
                                 :a
                                 [:name #".*func3-4"])
                     (mapv sdq/traverse-tree-dissoc-zipper)
                     ((t-utils/redact-file-fn [0 :children 0 :meta :file]
                                              [0 :children 0 :children 0 :meta :file]
                                              [0 :children 0 :children 0 :children 0 :meta :file]))))
        r
        => [{:arg-map nil
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
                                                        :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                                 :name 'com.billpiel.sayid.test.ns1/func3-4
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
                                            :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                     :name 'com.billpiel.sayid.test.ns1/func3-3
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
                                :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                         :name 'com.billpiel.sayid.test.ns1/func3-1
                         :path [:root10 :11]
                         :return 13
                         :started-at 0}]
             :depth 0
             :id :root10
             :path [:root10]
             :traced {:inner-fn #{}
                      :fn #{}
                      :ns #{'com.billpiel.sayid.test.ns1}}
             :ws-slot nil}])

      (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1))))

(fact-group "querying with q macro"
  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (sd/ws-clear-log!)
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (let [trace-root (sd/ws-add-trace-ns! com.billpiel.sayid.test.ns1)
          _ (com.billpiel.sayid.test.ns1/func3-1 3 8)
          trace (sd/ws-deref!)]

      (fact "find node by name and all parents"
        (->> (sd/w-q :a [:name #".*func3-4"])
             (mapv sdq/traverse-tree-dissoc-zipper)
             ((t-utils/redact-file-fn [0 :children 0 :meta :file]
                                      [0 :children 0 :children 0 :meta :file]
                                      [0 :children 0 :children 0 :children 0 :meta :file])))
        =>  [{:arg-map nil
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
                                                         :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                                  :name 'com.billpiel.sayid.test.ns1/func3-4
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
                                             :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                      :name 'com.billpiel.sayid.test.ns1/func3-3
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
                                 :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                          :name 'com.billpiel.sayid.test.ns1/func3-1
                          :path [:root10 :11]
                          :return 13
                          :started-at 0}]
              :depth 0
                            :id :root10
              :path [:root10]
              :traced {:inner-fn #{}
                       :fn #{}
                       :ns #{'com.billpiel.sayid.test.ns1}}
              :ws-slot nil}])
      (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1))))

(fact-group "trace a namespace by alias"
     (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
     (with-out-str (sd/ws-reset!))

     (with-redefs [sdt/now (t-utils/mock-now-fn)
                   gensym (t-utils/mock-gensym-fn)]
       (sd/ws-add-trace-ns! ns1)
       (com.billpiel.sayid.test.ns1/func1 :a)
       (let [trace (sd/ws-deref!)
             expected-trace {:arg-map nil
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
                                                            :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                                     :name 'com.billpiel.sayid.test.ns1/func2
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
                                                :ns (the-ns 'com.billpiel.sayid.test.ns1)}
                                         :name 'com.billpiel.sayid.test.ns1/func1
                                         :path [:root10 :11]
                                         :return :a
                                         :started-at 0}]
                             :depth 0
                             :id :root10
                             :path [:root10]
                             :traced {:inner-fn #{}
                                      :fn #{}
                                      :ns #{'com.billpiel.sayid.test.ns1}}
                             :ws-slot nil}]

         (fact "log is correct"
           (-> trace
               ((t-utils/redact-file-fn [:children 0 :meta :file]
                                        [:children 0 :children 0 :meta :file])))
           => expected-trace))

       (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)))


(fact-group "tracing then inner tracing a func results in only a inner trace"
  (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
  (with-out-str (sd/ws-reset!))

  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (let [original-func-complex com.billpiel.sayid.test.ns1/func-complex]
      (sd/ws-add-trace-fn! ns1/func-complex)

      (fact "trace is set in workspace"
        (:traced @sd/workspace) => {:fn #{'com.billpiel.sayid.test.ns1/func-complex}, :inner-fn #{}, :ns #{}}
                                        ;"{:ns #{}, :fn #{com.billpiel.sayid.test.ns1/func-complex}, :inner-fn #{}}\n"
        )

      (fact "trace is correct"
        (ns1/func-complex  2 3)
        (-> (sd/ws-deref!)
            :children
            ((t-utils/redact-file-fn [0 :meta :file])))
        => [{:arg-map {'a 2, 'b 3}
             :args [2 3]
             :children []
             :depth 1
             :ended-at 1
             :id :11
             :meta {:arglists '([a b])
                    :column 1
                    :file "FILE"
                    :line 63
                    :name 'func-complex
                    :ns (the-ns 'com.billpiel.sayid.test.ns1)}
             :name 'com.billpiel.sayid.test.ns1/func-complex
             :path [:root10 :11]
             :return [9 11 22 3]
             :started-at 0}])

      (sd/ws-clear-log!)

      (sd/ws-add-inner-trace-fn! ns1/func-complex)
      (fact "only inner trace is set in workspace"
        (:traced @sd/workspace) => {:fn #{}, :inner-fn #{'com.billpiel.sayid.test.ns1/func-complex}, :ns #{}})

      (fact "meta shows trace applied"
        (-> (meta #'ns1/func-complex)
            ((t-utils/redact-file-fn [:file])))
        => {:arglists '([a b])
            :column 1
            :file "FILE"
            :line 63
            :name 'func-complex
            :ns (the-ns 'com.billpiel.sayid.test.ns1)
            :com.billpiel.sayid.trace/traced [:root10 original-func-complex]
            :com.billpiel.sayid.trace/trace-type :inner-fn}))


    (with-out-str (sd/ws-remove-all-traces!))
    (sd/ws-clear-log!)

    (fact "all traces gone"
      (:traced @sd/workspace) => {:fn #{}, :inner-fn #{}, :ns #{}})

    (sdt/untrace-ns* 'com.billpiel.sayid.test.ns1)
    (with-out-str (sd/ws-reset!))))


(comment "
TODO
x tree query sibliings
x use dynamic ns for storage instead of atom/map?!?!?!
x save recordings
x save/load workspaces
x preserve arglists and other meta in core fns
  x use defn and docstring in core
x test reloading ws after code change
x optional print func log before and after children
x include ID in string output
x use 'returns' and 'returned' in output
x trace aliases
- ns wildcards
   x for `add-trace`
   - for `remove-trace`
   - other places?
x change query syntax -- single vector and get-some
x destructure arglist
  - filter out dummy __ args
x query evals (rather than gets) a `set`
x rendering strings is deadly slow?
- use pmap all over the place?
  -- add pmap mode to walk18???
x ws-load bug???
x profiling features
  x use joda
  x arg cardinality
x aliases for core fns
? have all core rec fns optionally take an arg
x split out and memoize arg map fn
  - was this actually an improvement???
- speed up
  x string out
  ~ load recording from ws
    x refactor deref
    - arg-map slow?
  x profiling
  x querying
x FIX adding a deep-trace to existing trace should undo trace before applying deep trace
- refator some things in string_output to multimethods?
x deep-trace :name should just be name. Do string concat in presentation logic
x test querying w/ deep trace
x safe print level/length for string out
x upgrade puget -- use seq-limit
- FIX adding only deep-trace throws execption on execution -- happened in scheduler
~ test profiling w/ deep trace
~ profile simulated tracer fn
x FIX return values of `false` aren't displayed
X should be able to query for func by func
x profiling interface in core
- FIX deep-traced fn returning fn blows up -- has no parent context -- throw explaining exception??
- rec-save-as should accept keyword or string
- never return ws or recs from core fns
x deep trace (inside fn)
x trace individual fns
x re-exec traces
x query selector funcs
  x special output
  x toggle output components
- switch to earlier clojure version -- deps too
  - get tests passing in all versions too
- query output by line/column position -- for emacs plugin
- cursors
   - bisect recording trees to find bugs
- split ns and fn name in trace rec and output
x some kind of string output length limit options?
- show file and line num in output
- wrap args that are funcs
 - and deep search values for funcs?
- wrap returns that are funcs
 - and deep search values for funcs?
- diff entries
- search fn syntax for dynamic vars to capture
- tag vals w meta data and track?
")
