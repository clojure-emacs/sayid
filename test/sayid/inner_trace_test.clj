(ns sayid.inner-trace-test
  "Characterization corpus for inner tracing.  Exercises a spread of Clojure
  constructs and pins down what inner tracing must capture: the traced function's
  own return value, the surface forms of the sub-expressions, and the value each
  one produced.  Assertions key on surface `:form` -> `:return` rather than exact
  tree shape, so they describe the behaviour rather than one implementation's
  macroexpansion, and survive swapping the instrumenter underneath."
  (:require [clojure.test :as t]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.inner-trace :as it]
            [sayid.inner-corpus :as c]
            [sayid.test-utils :as t-utils]))

;; This namespace characterizes the *legacy* rewriter specifically (its special
;; form/macro tags and loop/recur pseudo-nodes), so it pins the impl to :legacy
;; even though the default is now :ast.  The AST impl's behaviour lives in
;; sayid.inner-ast-test.  When the legacy rewriter goes, so does this file.
(defn- fixture [f]
  (with-out-str (sd/ws-reset!))
  (binding [it/*inner-trace-impl* :legacy]
    (with-redefs [sdt/now (t-utils/mock-now-fn)
                  gensym (t-utils/mock-gensym-fn)]
      (f)
      (with-out-str (sd/ws-reset!)))))

(t/use-fixtures :each fixture)

;;; ---- helpers ------------------------------------------------------------

(defn normalize
  "Project a recorded node to the fields that matter for characterization,
  dropping volatile ones (ids, paths, timings, meta, and `:args`, which can hold
  bare function objects for higher-order calls)."
  [node]
  {:form (:form node)
   :name (:name node)
   :return (:return node)
   :inner-tags (:inner-tags node)
   :threw? (boolean (:throw node))
   :children (mapv normalize (:children node))})

(defmacro capture
  "Reset the workspace, install the inner trace, run the body, and return the
  recorded inner-trace forest (the workspace roots), normalized."
  [add-form & body]
  `(do (with-out-str (sd/ws-reset!))
       ~add-form
       ~@body
       (->> (sd/ws-deref!) :children (mapv normalize))))

(defn nodes
  "Every node in the forest, depth-first."
  [forest]
  (mapcat #(tree-seq (constantly true) :children %) forest))

(defn forms
  "The set of surface forms captured anywhere in the forest."
  [forest]
  (set (map :form (nodes forest))))

(defn returns-for
  "Every recorded return value for a given surface form, in encounter order."
  [forest form]
  (->> (nodes forest)
       (filter #(= form (:form %)))
       (map :return)))

(defn return-for
  "The single recorded return for a form (asserts there's exactly one)."
  [forest form]
  (let [rs (returns-for forest form)]
    (t/is (= 1 (count rs)) (str "expected one capture of " (pr-str form)))
    (first rs)))

;;; ---- corpus -------------------------------------------------------------

(t/deftest arith-let-if
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
                        (t/is (= 26 (c/arith 6 7))))]
    (t/testing "captures the arithmetic sub-expressions with their values"
      (t/is (= 13 (return-for forest '(+ a b))))
      (t/is (= true (return-for forest '(> s 10))))
      (t/is (= 26 (return-for forest '(* s 2)))))
    (t/testing "tags the let and if special forms"
      (t/is (some #(and (= [:let] (:inner-tags %))) (nodes forest)))
      (t/is (some #(= [:if] (:inner-tags %)) (nodes forest))))))

(t/deftest threading-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/threaded)
                        (t/is (= 6 (c/threaded [1 2 3 4]))))]
    (t/testing "captures each step of the ->> pipeline (forms are the expanded, nested threads)"
      (t/is (= '(2 3 4 5) (return-for forest '(map inc xs))))
      (t/is (= '(2 4) (return-for forest '(filter even? (map inc xs)))))
      (t/is (= 6 (return-for forest '(reduce + 0 (filter even? (map inc xs)))))))
    (t/testing "tags the macro"
      (t/is (some #(= :macro (first (:inner-tags %))) (nodes forest))))))

(t/deftest cond-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/branchy)
                        (t/is (= :neg (c/branchy -3))))]
    (t/is (= true (return-for forest '(neg? n))))
    (t/is (contains? (forms forest) '(cond (neg? n) :neg (zero? n) :zero :else :pos)))))

(t/deftest when-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/whenny)
                        (t/is (= 6 (c/whenny 5))))]
    (t/is (= true (return-for forest '(pos? n))))
    (t/is (= 6 (return-for forest '(inc n))))))

(t/deftest loop-recur
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/looped)
                        (t/is (= 6 (c/looped 4))))]
    (t/testing "records every iteration's test and recur"
      (t/is (= [true true true true false] (returns-for forest '(< i n))))
      (t/is (= 4 (count (filter #(= [:recur] (:inner-tags %)) (nodes forest))))))
    (t/testing "captures the recur argument expressions"
      (t/is (= [1 2 3 4] (returns-for forest '(inc i)))))))

(t/deftest letfn-body
  ;; Regression for #14: instrumenting a letfn-using fn must not wrap the fn
  ;; bindings of the expanded letfn* form.
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/letfned)
                        (t/is (= 8 (c/letfned 3))))]
    (t/is (= 4 (return-for forest '(inc a))))))

(t/deftest case-macro
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/cased)
                        (t/is (= 2 (c/cased :b))))]
    ;; `case` expands to `case*`, whose surface form the current impl doesn't
    ;; recover, so it shows up only as a tagged macro node with the right value.
    (t/is (some #(and (= [:macro :case] (:inner-tags %))
                      (= 2 (:return %)))
                (nodes forest)))))

(t/deftest ^:pending try-catch-is-exception-transparent
  ;; Known legacy defect: `tr-fn` catches Throwable and returns nil, so an
  ;; inner-traced `try` never sees the exception and the `catch` never fires
  ;; (`caught` returns nil instead of :div-by-zero).  The AST instrumenter must
  ;; be exception-transparent; un-pend this once it is.
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/caught)
                        (t/is (= :div-by-zero (c/caught 0))))]
    (t/is (contains? (forms forest) '(/ 10 n)))))

(t/deftest destructuring-args
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/destructured)
                        (t/is (= 7 (c/destructured {:x 3 :y 4}))))]
    (t/is (= 7 (return-for forest '(+ x y))))))

(t/deftest multi-arity
  (let [forest (capture (sd/ws-add-inner-trace-fn! sayid.inner-corpus/multi)
                        (t/is (= 4 (c/multi 3)))
                        (t/is (= 5 (c/multi 2 3))))]
    ;; the 1-arg body delegates to the 2-arg one; the (+ a b) shows up
    (t/is (contains? (forms forest) '(+ a b)))))

(t/deftest nested-inner-and-outer
  (let [forest (capture (do (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
                            (sd/ws-add-inner-trace-fn! sayid.inner-corpus/threaded))
                        ;; threaded [2 2 2] -> (map inc) (3 3 3) -> (filter even?) () -> (reduce + 0) 0
                        ;; so arith 1 0 -> 1
                        (t/is (= 1 (c/nested-calls 1 2))))]
    ;; both inner-traced fns contribute their captures under the run
    (t/is (contains? (forms forest) '(map inc xs)))))
