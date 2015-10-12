(ns com.billpiel.mem-tracer.core-test
  (:require [midje.sweet :refer :all]
            [com.billpiel.mem-tracer.core :as mt]
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
                                        #inst "2010-01-01T04:00:00.000-00:00"]))

(def mock-gensym-fn (fn []
                      (make-mock-series-fn
                       (fn [id & [pre]]
                         (str (or pre "") id))
                       ["10" "11" "12" "13"])))

(defn remove-iso-ctrl [s]  (apply str (remove #(Character/isISOControl %) s)))

(fact-group "fact"

            (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)
            (with-redefs [mt/now (mock-now-fn)
                          gensym (mock-gensym-fn)]
              (let [trace-root (mt/trace-ns 'com.billpiel.mem-tracer.test.ns1)
                    _ (com.billpiel.mem-tracer.test.ns1/func1 :a)
                    trace (mt/deref-children trace-root)]

                (fact "log is correct"
                      trace
                      => {:children
                          [{:args [:a],
                            :children
                            [{:args [:a],
                              :children [],
                              :return :a,
                              :started-at #inst "2010-01-01T02:00:00.000-00:00",
                              :name "com.billpiel.mem-tracer.test.ns1/func2",
                              :id "12",
                              :parent-id "11",
                              :ended-at #inst "2010-01-01T03:00:00.000-00:00",
                              :depth 2}],
                            :return :a,
                            :started-at #inst "2010-01-01T01:00:00.000-00:00",
                            :name "com.billpiel.mem-tracer.test.ns1/func1",
                            :id "11",
                            :parent-id "root10",
                            :ended-at #inst "2010-01-01T04:00:00.000-00:00",
                            :depth 1}],
                          :depth 0,
                          :id "root10"})

                (fact "string output is correct"
                      (->> trace
                           mt/entry->string
                           remove-iso-ctrl)
                      => "[31m  [m[33m [33m| com.billpiel.mem-tracer.test.ns1/func1[m [33m| [33m:a[0m[m [33m| return => [33m:a[0m[32m [33m|[32m| com.billpiel.mem-tracer.test.ns1/func2[m [33m|[32m| [33m:a[0m[m [33m|[32m| return => [33m:a[0m [33m| return => [33m:a[0m")

                (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1))))

(def mock-log {:children
               [{:args [:a],
                 :children [],
                 :started-at #inst "2010-01-01T01:00:00.000-00:00",
                 :name "com.billpiel.mem-tracer.test.ns1/func-throws",
                 :id "11",
                 :parent-id "root10",
                 :ended-at #inst "2010-01-01T02:00:00.000-00:00",
                 :throw
                 {:cause "Exception from func-throws: :a",
                  :via
                  [{:type java.lang.Exception,
                    :at
                    {:method-name "invoke",
                     :file-name "ns1.clj",
                     :class-name "com.billpiel.mem_tracer.test.ns1$func_throws",
                     :line-number 14},
                    :message "Exception from func-throws: :a"}],
                  :trace
                  [{:method-name "invoke",
                    :file-name "ns1.clj",
                    :class-name "com.billpiel.mem_tracer.test.ns1$func_throws",
                    :line-number 14}
                   {:method-name "applyToHelper",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 154}
                   {:method-name "applyTo",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 144}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name "clojure.core$apply",
                    :line-number 624}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core$trace_fn_call$fn__22284",
                    :line-number 85}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name "com.billpiel.mem_tracer.core$trace_fn_call",
                    :line-number 83}
                   {:method-name "doInvoke",
                    :file-name "core.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core$trace_var_STAR_$fn__22290$tracing_wrapper__22291",
                    :line-number 120}
                   {:method-name "invoke",
                    :file-name "RestFn.java",
                    :class-name "clojure.lang.RestFn",
                    :line-number 408}
                   {:method-name "invoke",
                    :file-name "form-init2637533150160036371.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996$fn__22997$fn__22998",
                    :line-number 1}
                   {:method-name "invoke",
                    :file-name "form-init2637533150160036371.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996$fn__22997",
                    :line-number 1}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name "clojure.core$with_redefs_fn",
                    :line-number 6861}
                   {:method-name "invoke",
                    :file-name "form-init2637533150160036371.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995$fn__22996",
                    :line-number 1}
                   {:method-name "invoke",
                    :file-name "thread_safe_var_nesting.clj",
                    :class-name
                    "midje.util.thread_safe_var_nesting$with_altered_roots_STAR_",
                    :line-number 32}
                   {:method-name "invoke",
                    :file-name "form-init2637533150160036371.clj",
                    :class-name
                    "com.billpiel.mem_tracer.core_test$eval22994$fn__22995",
                    :line-number 1}
                   {:method-name "applyToHelper",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 152}
                   {:method-name "applyTo",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 144}
                   {:method-name "doInvoke",
                    :file-name "AFunction.java",
                    :class-name "clojure.lang.AFunction$1",
                    :line-number 29}
                   {:method-name "invoke",
                    :file-name "RestFn.java",
                    :class-name "clojure.lang.RestFn",
                    :line-number 397}
                   {:method-name "invoke",
                    :file-name "facts.clj",
                    :class-name "midje.checking.facts$check_one$fn__13965",
                    :line-number 31}
                   {:method-name "invoke",
                    :file-name "facts.clj",
                    :class-name "midje.checking.facts$check_one",
                    :line-number 30}
                   {:method-name "invoke",
                    :file-name "facts.clj",
                    :class-name "midje.checking.facts$creation_time_check",
                    :line-number 35}
                   {:method-name "invoke",
                    :file-name "form-init2637533150160036371.clj",
                    :class-name "com.billpiel.mem_tracer.core_test$eval22994",
                    :line-number 1}
                   {:method-name "eval",
                    :file-name "Compiler.java",
                    :class-name "clojure.lang.Compiler",
                    :line-number 6703}
                   {:method-name "eval",
                    :file-name "Compiler.java",
                    :class-name "clojure.lang.Compiler",
                    :line-number 6666}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name "clojure.core$eval",
                    :line-number 2927}
                   {:method-name "invoke",
                    :file-name "main.clj",
                    :class-name "clojure.main$repl$read_eval_print__6625$fn__6628",
                    :line-number 239}
                   {:method-name "invoke",
                    :file-name "main.clj",
                    :class-name "clojure.main$repl$read_eval_print__6625",
                    :line-number 239}
                   {:method-name "invoke",
                    :file-name "main.clj",
                    :class-name "clojure.main$repl$fn__6634",
                    :line-number 257}
                   {:method-name "doInvoke",
                    :file-name "main.clj",
                    :class-name "clojure.main$repl",
                    :line-number 257}
                   {:method-name "invoke",
                    :file-name "RestFn.java",
                    :class-name "clojure.lang.RestFn",
                    :line-number 1523}
                   {:method-name "invoke",
                    :file-name "interruptible_eval.clj",
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__6509",
                    :line-number 72}
                   {:method-name "applyToHelper",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 152}
                   {:method-name "applyTo",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 144}
                   {:method-name "invoke",
                    :file-name "core.clj",
                    :class-name "clojure.core$apply",
                    :line-number 624}
                   {:method-name "doInvoke",
                    :file-name "core.clj",
                    :class-name "clojure.core$with_bindings_STAR_",
                    :line-number 1862}
                   {:method-name "invoke",
                    :file-name "RestFn.java",
                    :class-name "clojure.lang.RestFn",
                    :line-number 425}
                   {:method-name "invoke",
                    :file-name "interruptible_eval.clj",
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$evaluate",
                    :line-number 56}
                   {:method-name "invoke",
                    :file-name "interruptible_eval.clj",
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__6551$fn__6554",
                    :line-number 191}
                   {:method-name "invoke",
                    :file-name "interruptible_eval.clj",
                    :class-name
                    "clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__6546",
                    :line-number 159}
                   {:method-name "run",
                    :file-name "AFn.java",
                    :class-name "clojure.lang.AFn",
                    :line-number 22}
                   {:method-name "runWorker",
                    :file-name "ThreadPoolExecutor.java",
                    :class-name "java.util.concurrent.ThreadPoolExecutor",
                    :line-number 1145}
                   {:method-name "run",
                    :file-name "ThreadPoolExecutor.java",
                    :class-name "java.util.concurrent.ThreadPoolExecutor$Worker",
                    :line-number 615}
                   {:method-name "run",
                    :file-name "Thread.java",
                    :class-name "java.lang.Thread",
                    :line-number 745}]},
                 :depth 1}],
               :depth 0,
               :id "root10"})

(fact-group "exception thrown"
            (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)
            (with-redefs [mt/now (mock-now-fn)
                          gensym (mock-gensym-fn)]
              (let [trace-root (mt/trace-ns 'com.billpiel.mem-tracer.test.ns1)
                    _ (try
                        (com.billpiel.mem-tracer.test.ns1/func-throws :a)
                        (catch Throwable t))
                    trace (mt/deref-children trace-root)]

                (fact "log is correct"
                      (dissoc trace :children)
                      => {:depth 0, :id "root10"})

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
                          :id "11"
                          :name "com.billpiel.mem-tracer.test.ns1/func-throws"
                          :parent-id "root10"
                          :started-at #inst "2010-01-01T01:00:00.000-00:00"})

                (fact "string output is correct"
                      (remove-iso-ctrl (mt/entry->string trace))
                      => "[31m  [m[33m [33m| com.billpiel.mem-tracer.test.ns1/func-throws[m [33m| [33m:a[0m[m [33m| [1;37;41mTHROW[m => [35m\"Exception from func-throws: :a\"[0m [33m| [31m[[0m[35m\"com.billpiel.mem_tracer.test.ns1$func_throws ns1.clj:14\"[0m[31m][0m[m")

                (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1))))

(comment "
TODO
- wrap args that are funcs
 - and deep search values for funcs?
- wrap returns that are funcs
 - and deep search values for funcs?
- string output like tools.trace
 - requires sequential log?
- diff entries
- optionally grab stactrace
")
