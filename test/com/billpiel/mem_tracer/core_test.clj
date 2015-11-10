(ns com.billpiel.mem-tracer.core-test
  (:require [midje.sweet :refer :all]
            [com.billpiel.mem-tracer.core :as mt]
            [com.billpiel.mem-tracer.trace :as mtt]
            [com.billpiel.mem-tracer.query :as mq]
            [com.billpiel.mem-tracer.util.tree-query :as mtq]
            com.billpiel.mem-tracer.test.ns1))

;; https://github.com/Prismatic/plumbing/blob/6f9f1b6453ed2c978a619dc99bb0317d8c053141/src/plumbing/core.cljx#L356
(defn swap-pair!
  "Like swap! but returns a pair [old-val new-val]"
  ([a f]
     (loop []
       (let [old-val @a
             new-val (f old-val)]
         (if (compare-and-set! a old-val new-val)
           [old-val new-val]
           (recur)))))
  ([a f & args]
   (swap-pair! a #(apply f % args))))

(defn make-mock-series-fn
  [f s]
  (let [a (atom s)]
    (fn [& args]
      (let [v (-> a
                  (swap-pair! subvec 1)
                  first
                  first)]
        (apply f (into [v] args))))))

(def mock-now-fn #(make-mock-series-fn identity
                                       [#inst "2010-01-01T01:00:00.000-00:00"
                                        #inst "2010-01-01T02:00:00.000-00:00"
                                        #inst "2010-01-01T03:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"]))



(def mock-gensym-fn (fn []
                      (make-mock-series-fn
                       (fn [id & [pre]]
                         (str (or pre "") id))
                       (vec (map str (range 10 1000))))))


(defn remove-iso-ctrl [s]  (apply str (remove #(Character/isISOControl %) s)))

(fact-group "basic test"
  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/reset-workspace!)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]

    (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)
    (com.billpiel.mem-tracer.test.ns1/func1 :a)
    (let [trace (mt/deref-workspace!)
          expected-trace {:children [{:args [:a]
                                      :children [{:args [:a]
                                                  :children []
                                                  :depth 2
                                                  :ended-at #inst "2010-01-01T03:00:00.000-00:00"
                                                  :id :12
                                                  :name "com.billpiel.mem-tracer.test.ns1/func2"
                                                  :path [:root10 :11 :12]
                                                  :return :a
                                                  :started-at #inst "2010-01-01T02:00:00.000-00:00"}]
                                      :depth 1
                                      :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                                      :id :11
                                      :name "com.billpiel.mem-tracer.test.ns1/func1"
                                      :path [:root10 :11]
                                      :return :a
                                      :started-at #inst "2010-01-01T01:00:00.000-00:00"}]
                          :depth 0
                          :id :root10
                          :path [:root10]
                          :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}}]

      (fact "log is correct"
        trace
        => expected-trace)

      (fact "string output is correct"
        (->> trace
             mt/entry->string
             remove-iso-ctrl)
        => "[31m [1;30;41m-[40m[31m[m[33m [33m|[1;30;43m-[40m[33mcom.billpiel.mem-tracer.test.ns1/func1[m [33m|  [33m:a[0m[m [33m| return =>  [33m|  [33m:a[0m[m[32m [33m|[32m|[1;30;42m-[40m[32mcom.billpiel.mem-tracer.test.ns1/func2[m [33m|[32m|  [33m:a[0m[m [33m|[32m| return =>  [33m|[32m|  [33m:a[0m[m [33m| return =>  [33m|  [33m:a[0m[m")

      (fact "remove trace"
        (mt/remove-trace-ns! 'com.billpiel.mem-tracer.test.ns1)
        (com.billpiel.mem-tracer.test.ns1/func1 :b)
        (mt/deref-workspace!) => (assoc expected-trace
                                        :traced #{})))

    (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)))

(fact-group "about enable/disable -all-traces!"
  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/reset-workspace!)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]

    (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)

    (fact "disable-all-traces! works"
      (mt/disable-all-traces!)
      (com.billpiel.mem-tracer.test.ns1/func1 :a)

      (mt/deref-workspace!)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}})

    (fact "enable-all-traces! works"
      (mt/enable-all-traces!)
      (com.billpiel.mem-tracer.test.ns1/func1 :a)

      (mt/deref-workspace!)
      => {:children [{:args [:a]
                      :children [{:args [:a]
                                  :children []
                                  :depth 2
                                  :ended-at #inst "2010-01-01T03:00:00.000-00:00"
                                  :id :12
                                  :name "com.billpiel.mem-tracer.test.ns1/func2"
                                  :path [:root10 :11 :12]
                                  :return :a
                                  :started-at #inst "2010-01-01T02:00:00.000-00:00"}]
                      :depth 1
                      :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                      :id :11
                      :name "com.billpiel.mem-tracer.test.ns1/func1"
                      :path [:root10 :11]
                      :return :a
                      :started-at #inst "2010-01-01T01:00:00.000-00:00"}]
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}})

    (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)))

(fact-group "about remove-all-traces!"
  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/reset-workspace!)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]

    (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)

    (fact "remove-all-traces! works"
      (mt/remove-all-traces!)
      (com.billpiel.mem-tracer.test.ns1/func1 :a)

      (mt/deref-workspace!)
      => {:children []
          :depth 0
          :id :root10
          :path [:root10]
          :traced #{}})

    (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)))

(def mock-log {:children
               [{:args [:a]
                 :children []
                 :started-at #inst "2010-01-01T01:00:00.000-00:00"
                 :name "com.billpiel.mem-tracer.test.ns1/func-throws"
                 :id :11
                 :parent-id :root10
                 :ended-at #inst "2010-01-01T02:00:00.000-00:00"
                 :throw
                 {:cause "Exception from func-throws: :a"
                  :via
                  [{:type java.lang.Exception
                    :at
                    {:method-name "invoke"
                     :file-name "ns1.clj"
                     :class-name "com.billpiel.mem_tracer.test.ns1$func_throws"
                     :line-number 14}
                    :message "Exception from func-throws: :a"}]
                  :trace
                  [{:method-name "invoke"
                    :file-name "ns1.clj"
                    :class-name "com.billpiel.mem_tracer.test.ns1$func_throws"
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
                    "com.billpiel.mem_tracer.core$trace_fn_call$fn__22284"
                    :line-number 85}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "com.billpiel.mem_tracer.core$trace_fn_call"
                    :line-number 83}
                   {:method-name "doInvoke"
                    :file-name "core.clj"
                    :class-name
                    "com.billpiel.mem_tracer.core$trace_var_STAR_$fn__22290$tracing_wrapper__22291"
                    :line-number 120}
                   {:method-name "invoke"
                    :file-name "RestFn.java"
                    :class-name "clojure.lang.RestFn"
                    :line-number 408}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996$fn__22997$fn__22998"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996$fn__22997"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "core.clj"
                    :class-name "clojure.core$with_redefs_fn"
                    :line-number 6861}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996"
                    :line-number 1}
                   {:method-name "invoke"
                    :file-name "thread_safe_var_nesting.clj"
                    :class-name
                    "midje.util.thread_safe_var_nesting$with_altered_roots_STAR_"
                    :line-number 32}
                   {:method-name "invoke"
                    :file-name "form-init2637533150160036371.clj"
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995"
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
                    :class-name "com.billpiel.mem_tracer.core_test$eval22994"
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

  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]
    (let [trace-root (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)
          _ (try
              (com.billpiel.mem-tracer.test.ns1/func-throws :a)
              (catch Throwable t))
          trace (mt/deref-workspace!)]

      (fact "log is correct"
        (dissoc trace :children)
        => {:depth 0
            :id :root10
            :path [:root10]
            :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}})

      (fact "log is correct"
        (-> trace :children count)
        => 1)

      (fact "log is correct"
        (-> trace :children first :throw :cause)
        => "Exception from func-throws: :a")

      (fact "log is correct"
        (-> trace :children first :throw :via first)
        => {:at {:class-name "com.billpiel.mem_tracer.test.ns1$func_throws"
                 :file-name "ns1.clj"
                 :line-number 14
                 :method-name "invoke"}
            :message "Exception from func-throws: :a"
            :type java.lang.Exception})

      (fact "log is correct"
        (-> trace :children first (dissoc :throw))
        => {:args [:a]
            :children []
            :depth 1
            :ended-at #inst "2010-01-01T02:00:00.000-00:00"
            :id :10
            :name "com.billpiel.mem-tracer.test.ns1/func-throws"
            :path [:root10 :10]
            :started-at #inst "2010-01-01T01:00:00.000-00:00"})

      (fact "string output is correct"
        (remove-iso-ctrl (mt/entry->string trace))
        => "[31m [1;37m>[31m[m[33m [33m|[1;37m>[33mcom.billpiel.mem-tracer.test.ns1/func-throws[m [33m|  [33m:a[0m[m [33m| [1;37;41mTHROW[m => [35m\"Exception from func-throws: :a\"[0m [33m| [31m[[0m[35m\"com.billpiel.mem_tracer.test.ns1$func_throws ns1.clj:14\"[0m[31m][0m[m")

      (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1))))

(fact-group "querying with query/query"

  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/clear-log!)
  (mt/reset-workspace!)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]
    (let [trace-root (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)
          _ (com.billpiel.mem-tracer.test.ns1/func3-1 3 8)
          trace (mt/deref-workspace!)]

      (fact "find node by name and all parents"
        (mtq/query (mq/trace->zipper trace)
                   {:a (mq/mk-query-fn [:name] #".*func3-4") }
                   (some-fn (mtq/has-all-tags-fn :a)
                            (mtq/has-descen-fn :a)))
        => [{:children [{:args [3 8]
                         :children [{:args [8]
                                     :children [{:args [8]
                                                 :children []
                                                 :depth 3
                                                 :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                                                 :id :15
                                                 :name "com.billpiel.mem-tracer.test.ns1/func3-4"
                                                 :path [:root10 :11 :13 :15]
                                                 :return 8
                                                 :started-at #inst "2010-01-01T04:00:00.000-00:00"}]
                                     :depth 2
                                     :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                                     :id :13
                                     :name "com.billpiel.mem-tracer.test.ns1/func3-3"
                                     :path [:root10 :11 :13]
                                     :return 8
                                     :started-at #inst "2010-01-01T04:00:00.000-00:00"}]
                         :depth 1
                         :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                         :id :11
                         :name "com.billpiel.mem-tracer.test.ns1/func3-1"
                         :path [:root10 :11]
                         :return 13
                         :started-at #inst "2010-01-01T01:00:00.000-00:00"}]
             :depth 0
             :id :root10
             :path [:root10]
             :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}}])

      (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1))))


(fact-group "querying with q macro"

  (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1)
  (mt/clear-log!)
  (mt/reset-workspace!)
  (with-redefs [mtt/now (mock-now-fn)
                gensym (mock-gensym-fn)]
    (let [trace-root (mt/add-trace-ns! 'com.billpiel.mem-tracer.test.ns1)
          _ (com.billpiel.mem-tracer.test.ns1/func3-1 3 8)
          trace (mt/deref-workspace!)]

      (fact "find node by name and all parents"
        (mt/q a* [:name] #".*func3-4")
        =>  [{:path [:root10]
              :children
              [{:args [3 8]
                :path [:root10 :11]
                :children
                [{:args [8]
                  :path [:root10 :11 :13]
                  :children
                  [{:args [8]
                    :path [:root10 :11 :13 :15]
                    :children []
                    :return 8
                    :started-at #inst "2010-01-01T04:00:00.000-00:00"
                    :name "com.billpiel.mem-tracer.test.ns1/func3-4"
                    :id :15
                    :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                    :depth 3}]
                  :return 8
                  :started-at #inst "2010-01-01T04:00:00.000-00:00"
                  :name "com.billpiel.mem-tracer.test.ns1/func3-3"
                  :id :13
                  :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                  :depth 2}]
                :return 13
                :started-at #inst "2010-01-01T01:00:00.000-00:00"
                :name "com.billpiel.mem-tracer.test.ns1/func3-1"
                :id :11
                :ended-at #inst "2010-01-01T04:00:00.000-00:00"
                :depth 1}]
              :id :root10
              :depth 0
              :traced #{[:ns 'com.billpiel.mem-tracer.test.ns1]}}])
      (mtt/untrace-ns* 'com.billpiel.mem-tracer.test.ns1))))


(comment "
TODO
- nice query result printing
- nicer printing (rewrite string output ns?)
- diff entries
- save/load workspaces
- wrap args that are funcs
 - and deep search values for funcs?
- wrap returns that are funcs
 - and deep search values for funcs?
- re-exec traces
- deep trace
")
