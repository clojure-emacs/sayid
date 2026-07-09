(ns sayid.data-test
  (:require [clojure.test :as t]
            [sayid.core :as sd]
            [sayid.data :as data]
            [sayid.inner-corpus :as c]))

(defn- fixture [f]
  (with-out-str (sd/ws-reset!))
  (f)
  (with-out-str (sd/ws-reset!)))

(t/use-fixtures :each fixture)

(t/deftest trace-data-keeps-live-values
  (sd/ws-add-trace-ns! sayid.inner-corpus)
  (c/nested-calls 1 2)
  (let [d (data/trace-data)
        root (first d)]
    (t/is (= 1 (count d)))
    (t/testing "keyword keys and live (not pr-str'd) captured values"
      (t/is (= 'sayid.inner-corpus/nested-calls (:name root)))
      (t/is (= [1 2] (:args root)))
      (t/is (= 1 (:return root))))
    (t/testing "the nested calls come through as children"
      (t/is (= '[sayid.inner-corpus/threaded sayid.inner-corpus/arith]
               (map :name (:children root)))))
    (t/testing "timing and source location are surfaced"
      (t/is (number? (:ms root)))
      (t/is (= "sayid/inner_corpus.clj" (get-in root [:source :file]))))))

(t/deftest trace-data-includes-inner-expression-values
  (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
  (c/arith 6 7)
  (let [root (first (data/trace-data))
        forms (map :form (:children root))]
    (t/is (= '[(+ a b) (> s 10) (* s 2)] forms))
    (t/is (= [13 true 26] (map :return (:children root))))))

(t/deftest tap-trace-taps-the-data
  (let [p (promise)
        tapfn (fn [x] (deliver p x))]
    (add-tap tapfn)
    (try
      (sd/ws-add-trace-ns! sayid.inner-corpus)
      (c/nested-calls 1 2)
      (t/is (= 1 (data/tap-trace!)))
      (let [tapped (deref p 2000 ::timeout)]
        (t/is (not= ::timeout tapped) "tap> delivered")
        (t/is (= 'sayid.inner-corpus/nested-calls (-> tapped first :name))))
      (finally (remove-tap tapfn)))))
