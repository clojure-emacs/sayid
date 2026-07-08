(ns sayid.inner-ast-test
  "Drives the tools.analyzer.jvm-based inner-tracing instrumenter (the `:ast`
  impl).  Asserts the invariants that matter - the traced fn's own value, the
  surface form and value of each captured sub-expression, and exception
  transparency - rather than the legacy impl's exact node taxonomy."
  (:require [clojure.test :as t]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.inner-corpus :as c]
            [sayid.test-utils :as t-utils]))

(defn- fixture [f]
  (with-out-str (sd/ws-reset!))
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (with-out-str (sd/ws-reset!))))

(t/use-fixtures :each fixture)

(defn normalize [node]
  {:form (:form node)
   :name (:name node)
   :return (:return node)
   :threw? (boolean (:throw node))
   :children (mapv normalize (:children node))})

(defmacro capture [add-form & body]
  `(do (with-out-str (sd/ws-reset!))
       ~add-form
       ~@body
       (->> (sd/ws-deref!) :children (mapv normalize))))

(defn nodes [forest]
  (mapcat #(tree-seq (constantly true) :children %) forest))

(defn forms [forest]
  (set (map :form (nodes forest))))

(defn returns-for [forest form]
  (->> (nodes forest) (filter #(= form (:form %))) (map :return)))

(defn return-for [forest form]
  (let [rs (returns-for forest form)]
    (t/is (= 1 (count rs)) (str "expected one capture of " (pr-str form)))
    (first rs)))

;;; ---- corpus under the AST impl -----------------------------------------

(t/deftest arith-let-if
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
                        (t/is (= 26 (c/arith 6 7))))]
    (t/is (= 13 (return-for forest '(+ a b))))
    (t/is (= true (return-for forest '(> s 10))))
    (t/is (= 26 (return-for forest '(* s 2))))))

(t/deftest threading-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/threaded)
                        (t/is (= 6 (c/threaded [1 2 3 4]))))]
    (t/is (= '(2 3 4 5) (return-for forest '(map inc xs))))
    (t/is (= '(2 4) (return-for forest '(filter even? (map inc xs)))))
    (t/is (= 6 (return-for forest '(reduce + 0 (filter even? (map inc xs))))))))

(t/deftest cond-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/branchy)
                        (t/is (= :neg (c/branchy -3))))]
    (t/is (= true (return-for forest '(neg? n))))))

(t/deftest when-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/whenny)
                        (t/is (= 6 (c/whenny 5))))]
    (t/is (= true (return-for forest '(pos? n))))
    (t/is (= 6 (return-for forest '(inc n))))))

(t/deftest loop-recur
  ;; The AST impl doesn't synthesize loop/recur pseudo-nodes (that hand-rolled
  ;; machinery is what made the legacy impl fragile).  It captures the real
  ;; sub-expressions - the test and the recur arguments - and, crucially, the
  ;; loop still computes correctly.
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/looped)
                        (t/is (= 6 (c/looped 4))))]
    (t/is (= [true true true true false] (returns-for forest '(< i n))))
    (t/is (= [1 2 3 4] (returns-for forest '(inc i))))
    (t/is (= [0 1 3 6] (returns-for forest '(+ acc i))))))

(t/deftest letfn-body
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/letfned)
                        (t/is (= 8 (c/letfned 3))))]
    (t/is (= 4 (return-for forest '(inc a))))))

(t/deftest case-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/cased)
                        (t/is (= 2 (c/cased :b))))]
    (t/is (some #(= 2 (:return %)) (nodes forest)))))

(t/deftest try-catch-is-exception-transparent
  ;; The headline correctness win over the legacy impl: an inner-traced try/catch
  ;; still sees the exception, so the catch fires and the value is right.
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/caught)
                        (t/is (= :div-by-zero (c/caught 0))))]
    (t/testing "the throwing sub-expression is recorded as having thrown"
      (t/is (some #(and (= '(/ 10 n) (:form %)) (:threw? %)) (nodes forest))))))

(t/deftest destructuring-args
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/destructured)
                        (t/is (= 7 (c/destructured {:x 3 :y 4}))))]
    (t/is (= 7 (return-for forest '(+ x y))))))

(t/deftest multi-arity
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/multi)
                        (t/is (= 4 (c/multi 3)))
                        (t/is (= 5 (c/multi 2 3))))]
    (t/is (contains? (forms forest) '(+ a b)))))

(t/deftest nested-inner-and-outer
  (let [forest (capture (do (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
                            (sd/ws-add-inner-trace-fn! sayid.inner-corpus/threaded))
                        (t/is (= 1 (c/nested-calls 1 2))))]
    (t/is (contains? (forms forest) '(map inc xs)))))
