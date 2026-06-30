(ns sayid.public-api-test
  "Characterization tests that pin `core`'s public surface ahead of the
  core decouple/cleanup on the roadmap (see doc/roadmap.md, initiative 1).

  The engine internals are already well covered by `core-test`, `query-test`
  and `workspace-test`.  This namespace adds the pieces that refactor needs
  most and that were missing: a guard on the public API surface itself, and
  characterizations of rendering and profiling - two public paths the refactor
  touches (`string-output`, `profiling`) that had no coverage."
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.test-utils :as t-utils]
            [sayid.test-ns1 :as ns1]))

(defn- fixture
  [f]
  (sdt/untrace-ns* 'sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  ;; Mock the clock and gensym so timestamps and ids are deterministic, exactly
  ;; like `core-test` does.
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (sdt/untrace-ns* 'sayid.test-ns1)))

(t/use-fixtures :each fixture)

;;; --- Public API surface -----------------------------------------------------

(def ^:private public-entry-points
  "The `core` vars that clients depend on - the nREPL middleware and the REPL
  workflow documented in the README.  The roadmap renames namespaces behind
  deprecated aliases; these must keep resolving through that work."
  '[ws-reset! ws-clear-log! ws-deref! ws-show-traced ws-show-traced*
    ws-add-trace-fn! ws-add-inner-trace-fn! ws-add-trace-ns! ws-remove-trace-ns!
    ws-remove-trace-fn! ws-remove-all-traces!
    ws-enable-all-traces! ws-disable-all-traces! ws-cycle-all-traces!
    ws-enable-trace-fn! ws-disable-trace-fn!
    ws-enable-trace-ns! ws-disable-trace-ns!
    ws-query ws-query* ws-query-print ws-query-by-file-line ws-query-by-file-pos
    ws-print ws-view! set-view! with-view
    ws-save! ws-load!
    pro-analyze pro-net-time pro-gross-repeats
    version])

(t/deftest public-entry-points-resolve
  (t/testing "every documented public entry point still resolves in core"
    (doseq [sym public-entry-points]
      (t/is (some? (ns-resolve 'sayid.core sym))
            (str sym " must remain part of core's public API")))))

;;; --- Rendering (string-output) ---------------------------------------------

(defn- strip-ansi
  [s]
  (str/replace s #"\[[0-9;]*m" ""))

(defn- render-active-ws
  "Trace ns1, run THUNK, and return the default `ws-print` rendering with ANSI
  colouring stripped."
  [thunk]
  (sd/ws-add-trace-ns! ns1)
  (thunk)
  (strip-ansi (with-out-str (sd/ws-print))))

(t/deftest rendering-characterization
  (t/testing "ws-print renders the traced call tree"
    (let [out (render-active-ws #(ns1/func1 :a))]
      (t/testing "; names the traced functions"
        (t/is (str/includes? out "func1"))
        (t/is (str/includes? out "func2")))
      (t/testing "; shows the argument/return value"
        (t/is (str/includes? out ":a")))
      (t/testing "; is a non-trivial multi-line tree"
        (t/is (< 1 (count (str/split-lines out))))))))

;;; --- Profiling --------------------------------------------------------------

(t/deftest profiling-characterization
  (t/testing "pro-analyze assocs a :profile keyed by each traced fn that ran"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func3-1 3 8)
    (let [profile (:profile (sd/pro-analyze (sd/ws-deref!)))]
      (t/testing "; covers the entry fn and a callee (keyed by namespaced keyword)"
        (t/is (contains? profile :sayid.test-ns1/func3-1))
        (t/is (contains? profile :sayid.test-ns1/func3-2)))
      (t/testing "; the entry fn ran exactly once"
        (t/is (= 1 (:count (get profile :sayid.test-ns1/func3-1)))))
      (t/testing "; records call counts and timing metrics"
        (let [entry (get profile :sayid.test-ns1/func3-2)]
          (t/is (pos-int? (:count entry)))
          (t/is (number? (:net-time-sum entry)))
          (t/is (number? (:gross-time-sum entry))))))))
