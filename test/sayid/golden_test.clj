(ns sayid.golden-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [sayid.core :as sd]
            [sayid.golden :as gold]
            [sayid.inner-corpus :as c]))

(defn- fixture [f]
  (with-out-str (sd/ws-reset!))
  (f)
  (with-out-str (sd/ws-reset!)))

(t/use-fixtures :each fixture)

;;; ---- the mechanism, against a throwaway directory ----------------------

(t/deftest golden-mechanism
  (let [dir (str "target/golden-test-" (System/nanoTime))]
    (binding [gold/*golden-dir* dir
              gold/*update* false]
      (let [data [{:name "f" :args ["1"] :return "1"}]]
        (t/testing "first check creates the golden"
          (t/is (= :created (:status (gold/check-golden "m" data)))))
        (t/testing "matching data matches"
          (t/is (= :match (:status (gold/check-golden "m" data)))))
        (t/testing "changed data is a mismatch, with a diff"
          (let [r (gold/check-golden "m" [{:name "f" :args ["1"] :return "2"}])]
            (t/is (= :mismatch (:status r)))
            (t/is (= [{:return "1"}] (first (:diff r))))
            (t/is (= [{:return "2"}] (second (:diff r))))))
        (t/testing "update mode rewrites the golden"
          (binding [gold/*update* true]
            (t/is (= :updated (:status (gold/check-golden "m" [{:name "g"}])))))
          (t/is (= [{:name "g"}] (gold/read-golden "m")))))
      (doseq [f (reverse (file-seq (io/file dir)))] (.delete f)))))

;;; ---- worked examples, against committed goldens ------------------------

(t/deftest outer-trace-golden
  ;; The recorded call tree of an outer trace: which functions ran, with what
  ;; arguments and return values, and how they nested.
  (sd/ws-add-trace-ns! sayid.inner-corpus)
  (c/nested-calls 1 2)
  (t/is (gold/matches-golden? "nested-calls-outer")))

(t/deftest inner-trace-golden
  ;; The differentiator: the golden captures every intermediate expression value
  ;; *inside* the function, not just the call and its result.
  (sd/ws-add-inner-trace-fn! sayid.inner-corpus/arith)
  (c/arith 6 7)
  (t/is (gold/matches-golden? "arith-inner")))

;;; ---- diffing two traces ------------------------------------------------

(defn- trace-of [& args]
  (with-out-str (sd/ws-reset!))
  (sd/ws-add-trace-ns! sayid.inner-corpus)
  (apply c/nested-calls args)
  (gold/golden-trace))

(t/deftest diff-traces-identical-is-empty
  (t/is (= [] (gold/diff-traces (trace-of 1 2) (trace-of 1 2)))))

(t/deftest diff-traces-pinpoints-changes
  ;; nested-calls 1 2 vs 2 2: threaded [2 2 2] is identical either way, but arith
  ;; is called with different args and nested-calls returns differently.
  (let [d (gold/diff-traces (trace-of 1 2) (trace-of 2 2))
        root (first d)]
    (t/is (= 1 (count d)))
    (t/is (= :changed (:status root)))
    (t/testing "the top call's args and return changed"
      (t/is (= [["1" "2"] ["2" "2"]] (get-in root [:changes :args])))
      (t/is (= ["1" "2"] (get-in root [:changes :return]))))
    (t/testing "only the arith child is reported as changed; threaded is pruned"
      (t/is (= ["sayid.inner-corpus/arith"] (map :name (:children root))))
      (t/is (= [["1" "0"] ["2" "0"]] (get-in root [:children 0 :changes :args]))))))
