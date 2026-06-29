(ns com.billpiel.sayid.nrepl-middleware-test
  "Smoke tests for the nREPL middleware wiring.

  These don't exercise the ops end to end (that needs a live nREPL session),
  but they guard against the middleware namespace failing to load or the
  descriptor drifting out of sync with the registered ops - the kinds of
  breakage that are easy to introduce and annoying to debug."
  (:require [clojure.test :as t]
            [com.billpiel.sayid.nrepl-middleware :as mw]
            [nrepl.transport :as transport]))

(defn- recording-transport
  "An nREPL transport that records every sent message into ATOM."
  [sent]
  (reify transport/Transport
    (recv [this] this)
    (recv [this _timeout] this)
    (send [this msg] (swap! sent conj msg) this)))

(t/deftest ops-are-registered
  (t/is (seq mw/sayid-nrepl-ops)
        "at least one op should be registered")
  (t/is (every? string? (keys mw/sayid-nrepl-ops))
        "ops are keyed by their string name")
  (t/is (every? var? (vals mw/sayid-nrepl-ops))
        "ops resolve to vars"))

(t/deftest wrap-sayid-is-a-middleware
  (t/is (fn? mw/wrap-sayid))
  (t/testing "unknown ops are passed through to the wrapped handler"
    (let [seen (atom nil)
          handler (mw/wrap-sayid (fn [msg] (reset! seen msg) :handled))]
      (t/is (= :handled (handler {:op "something-not-ours"})))
      (t/is (= {:op "something-not-ours"} @seen)))))

(t/deftest failing-op-replies-with-error-status
  (t/testing "a Sayid op that throws still terminates the op with an error status"
    (let [sent (atom [])
          handler (mw/wrap-sayid (fn [_] :unhandled))
          ;; A non-existent file makes `sayid-trace-ns-in-file' throw while
          ;; slurping, exercising the error path.
          msg {:op "sayid-trace-ns-in-file"
               :file "/sayid/does-not-exist.clj"
               :transport (recording-transport sent)}]
      (handler msg)
      (let [statuses (->> @sent (keep :status) (apply concat) set)]
        (t/is (contains? statuses :error)
              "the response carries an :error status")
        (t/is (contains? statuses :done)
              "the op is still terminated with :done so the client doesn't hang")))))

(t/deftest descriptor-matches-ops
  (let [descriptor (:nrepl.middleware/descriptor (meta #'mw/wrap-sayid))]
    (t/is (some? descriptor)
          "set-descriptor! was called on wrap-sayid")
    (t/is (= (set (keys mw/sayid-nrepl-ops))
             (set (keys (:handles descriptor))))
          "every registered op is advertised in the descriptor")))
