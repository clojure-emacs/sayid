(ns sayid.inner-diff-test
  (:require [clojure.test :as t]
            [clojure.set :as set]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.inner-trace :as it]
            [sayid.inner-corpus]
            [sayid.test-utils :as t-utils]))

(def cases
  "[fn-symbol args expected-return]"
  '[[sayid.inner-corpus/arith [6 7] 26]
    [sayid.inner-corpus/threaded [[1 2 3 4]] 6]
    [sayid.inner-corpus/branchy [-3] :neg]
    [sayid.inner-corpus/whenny [5] 6]
    [sayid.inner-corpus/looped [4] 6]
    [sayid.inner-corpus/letfned [3] 8]
    [sayid.inner-corpus/cased [:b] 2]
    [sayid.inner-corpus/caught [0] :div-by-zero]
    [sayid.inner-corpus/destructured [{:x 3 :y 4}] 7]
    [sayid.inner-corpus/multi [3] 4]
    [sayid.inner-corpus/nested-calls [1 2] 1]])

(defn- nodes [forest]
  (mapcat #(tree-seq (constantly true) :children %) forest))

(defn- call-captures
  "Set of [surface-form value] for plain function-call nodes - the ones without
  :inner-tags (so, in the legacy tree, excluding its let/if/macro/recur pseudo
  nodes).  This is the semantic core both impls should agree on."
  [forest]
  (->> (nodes forest)
       (remove :inner-tags)
       (filter #(seq? (:form %)))
       (map (juxt :form :return))
       set))

(defn- run [impl fsym args]
  (sdt/untrace-ns* 'sayid.inner-corpus)
  (with-out-str (sd/ws-reset!))
  (binding [it/*inner-trace-impl* impl]
    (sd/ws-add-inner-trace-fn!* fsym))
  (let [result (try (apply @(resolve fsym) args)
                    (catch Throwable t [:threw (str t)]))
        caps   (call-captures (:children (sd/ws-deref!)))]
    (sdt/untrace-ns* 'sayid.inner-corpus)
    {:result result :caps caps}))

(t/deftest ast-is-behaviourally-equivalent-to-legacy
  ;; The safety property for making the AST impl the default: instrumenting a
  ;; function must not change what it returns, and the AST impl must capture at
  ;; least the sub-expression values the legacy impl does.  The two intended
  ;; divergences are asserted explicitly: `caught` (legacy swallows the exception,
  ;; the AST impl is transparent) and `letfned` (the AST impl also captures inside
  ;; the letfn body, which the legacy impl skips).
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (doseq [[fsym args expected] cases]
      (let [leg (run :legacy fsym args)
            ast (run :ast fsym args)]
        (t/testing (str fsym)
          (t/is (= expected (:result ast))
                "the AST impl computes the right value")
          (if (= 'sayid.inner-corpus/caught fsym)
            (do (t/is (= nil (:result leg))
                      "legacy swallows the exception (the bug this fixes)")
                (t/is (= :div-by-zero (:result ast))
                      "the AST impl is exception-transparent"))
            (t/is (= (:result leg) (:result ast))
                  "both impls yield the same result"))
          (t/is (set/subset? (:caps leg) (:caps ast))
                "the AST impl captures a superset of legacy's call values")
          (when (= 'sayid.inner-corpus/letfned fsym)
            (t/is (contains? (:caps ast) '[(* 2 n) 8])
                  "and, unlike legacy, captures inside the letfn body")))))))
