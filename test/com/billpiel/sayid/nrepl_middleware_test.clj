(ns com.billpiel.sayid.nrepl-middleware-test
  "Tests for the nREPL middleware.

  Two parts: the wiring (ops registered, descriptor in sync, errors terminated
  instead of hanging), and a characterization of the current wire format - the
  response shapes and the `clj->nrepl` bencode encoding.  That second part is
  the safety net for initiative 3 (return data, not pre-rendered strings), which
  will change these shapes deliberately; see doc/roadmap.md.

  Ops are driven through `wrap-sayid` with a recording transport and a populated
  workspace, so no live nREPL session is needed."
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [com.billpiel.sayid.nrepl-middleware :as mw]
            [com.billpiel.sayid.core :as sd]
            [com.billpiel.sayid.trace :as sdt]
            [com.billpiel.sayid.test-utils :as t-utils]
            [com.billpiel.sayid.test-ns1 :as ns1]
            [nrepl.transport :as transport]
            [nrepl.bencode :as bencode]))

(defn- recording-transport
  "An nREPL transport that records every sent message into ATOM."
  [sent]
  (reify transport/Transport
    (recv [this] this)
    (recv [this _timeout] this)
    (send [this msg] (swap! sent conj msg) this)))

(defn- fixture
  [f]
  (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  ;; Deterministic clock and ids, exactly like core-test and public-api-test.
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (sdt/untrace-ns* 'com.billpiel.sayid.test-ns1)))

(t/use-fixtures :each fixture)

(t/deftest ops-are-registered
  (t/is (seq mw/sayid-nrepl-ops)
        "at least one op should be registered")
  (t/is (every? string? (keys mw/sayid-nrepl-ops))
        "ops are keyed by their string name")
  (t/is (every? var? (vals mw/sayid-nrepl-ops))
        "ops resolve to vars"))

(t/deftest consolidated-trace-ops-registered
  (t/is (every? (set (keys mw/sayid-nrepl-ops))
                ["sayid-trace-fn" "sayid-trace-fn-at-point"
                 "sayid-trace-ns" "sayid-all-traces"])
        "the action-parametrized trace ops are registered"))

(t/deftest unknown-action-is-an-error-not-a-hang
  (t/testing "an unrecognized action yields an error status, still terminated"
    (let [sent (atom [])
          handler (mw/wrap-sayid (fn [_] :unhandled))]
      (handler {:op "sayid-trace-ns" :action "bogus" :ns "whatever"
                :transport (recording-transport sent)})
      (let [statuses (->> @sent (keep :status) (apply concat) set)]
        (t/is (contains? statuses :error))
        (t/is (contains? statuses :done))))))

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

;;; --- Wire-format characterization -------------------------------------------
;;; Pin the shapes initiative 3 will change, so the change is deliberate and the
;;; Emacs client keeps working until it migrates.

(defn- captured-value
  "Drive OP through `wrap-sayid` with MSG and return the `:value` of the first
  response that carries one."
  [op msg]
  (let [sent (atom [])
        handler (mw/wrap-sayid (fn [_] :unhandled))]
    (handler (assoc msg :op op :transport (recording-transport sent)))
    (some :value @sent)))

(t/deftest clj->nrepl-prepares-values-for-bencode
  (t/testing "scalars"
    (t/is (= "kw" (mw/clj->nrepl :kw)) "keywords become their bare name")
    (t/is (= 1 (mw/clj->nrepl true)) "true becomes 1")
    (t/is (= nil (mw/clj->nrepl false)) "false becomes nil")
    (t/is (= 42 (mw/clj->nrepl 42)) "numbers pass through")
    (t/is (= "s" (mw/clj->nrepl "s")) "strings pass through"))
  (t/testing "collections become seqs, recursively"
    (t/is (= '(1 2 3) (mw/clj->nrepl [1 2 3])) "vectors become seqs")
    (t/is (= '(("a" 1) ("b" (1 "c")))
             (mw/clj->nrepl {:a 1 :b [true :c]}))
          "maps become seqs of [k v] seqs, with keys and vals encoded too")))

(t/deftest get-workspace-returns-the-rendered-trio
  (t/testing "sayid-get-workspace ships [text properties query-args], not data"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (let [value (captured-value "sayid-get-workspace" {})
          [text props query-args] value]
      (t/is (= 3 (count value))
            "the value is the three-element trio")
      (t/testing "; element 0 is the server-rendered text"
        (t/is (string? text))
        (t/is (str/includes? text "func1"))
        (t/is (str/includes? text "func2")))
      (t/testing "; element 1 is the text-property spans"
        (t/is (coll? props)))
      (t/testing "; element 2 is the pr-str'd query args (nil here)"
        (t/is (= "nil" query-args))))))

(t/deftest version-op-returns-the-version-string
  (t/is (= sd/version (captured-value "sayid-version" {}))))

;;; --- Data ops (initiative 3) ------------------------------------------------

(defn- bencode-roundtrips?
  "True if V survives a real bencode write then read - the encodability check the
  recording transport can't do, since it never serializes."
  [v]
  (let [out (java.io.ByteArrayOutputStream.)]
    (bencode/write-bencode out v)
    (-> (java.io.ByteArrayInputStream. (.toByteArray out))
        java.io.PushbackInputStream.
        bencode/read-bencode
        some?)))

(t/deftest get-workspace-data-returns-a-native-tree
  (t/testing "sayid-get-workspace-data ships the call tree as data, not a trio"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (let [value (captured-value "sayid-get-workspace-data" {})
          root (first value)]
      (t/is (= 1 (count value)) "one root call was recorded")
      (t/testing "; structural fields are native and navigable"
        (t/is (map? root))
        (t/is (str/includes? (get root "name") "func1"))
        (t/is (int? (get root "depth")))
        (t/is (vector? (get root "children"))))
      (t/testing "; captured values are pr-str'd"
        (t/is (= ":a" (get root "return")))
        (t/is (= [":a"] (get root "args")))
        (t/is (= {"arg1" ":a"} (get root "arg-map"))))
      (t/testing "; children are nested data nodes"
        (t/is (str/includes? (get (first (get root "children")) "name")
                             "func2"))))))

(t/deftest get-workspace-data-is-wire-encodable
  (t/testing "the value bencodes cleanly - no nils or unencodable values"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (t/is (bencode-roundtrips? (captured-value "sayid-get-workspace-data" {})))))

(t/deftest node->data-captures-throws
  (t/testing "a node that threw carries a trimmed throw and no return"
    (let [data (mw/node->data
                {:id :7 :name 'ns/boom :depth 1 :args [7] :arg-map '{x 7}
                 :children []
                 :throw {:cause "boom"
                         :via [{:type 'clojure.lang.ExceptionInfo
                                :message "boom"}]
                         :data {:x 7}}})]
      (t/is (not (contains? data "return"))
            "a thrown call has no return value")
      (let [thrown (get data "throw")]
        (t/is (= "boom" (get thrown "cause")))
        (t/is (= "clojure.lang.ExceptionInfo" (get thrown "class")))
        (t/is (= "{:x 7}" (get thrown "data")) "ex-data is pr-str'd")))))

(t/deftest rendered-query-by-fn-still-returns-the-trio
  (t/testing "splitting query-building out of sayid-query-by-fn keeps it rendering"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (let [value (captured-value "sayid-query-by-fn"
                                {:fn-name "com.billpiel.sayid.test-ns1/func2"
                                 :mod ""})
          [text props query-args] value]
      (t/is (= 3 (count value)) "still the three-element trio")
      (t/is (string? text))
      (t/is (str/includes? text "func2"))
      (t/is (coll? props))
      (t/is (string? query-args)))))

(t/deftest query-data-ops-return-node-data
  (sd/ws-add-trace-ns! ns1)
  (ns1/func1 :a)
  (t/testing "sayid-query-by-fn-data returns the matched calls as data"
    (let [value (captured-value "sayid-query-by-fn-data"
                                {:fn-name "com.billpiel.sayid.test-ns1/func2"
                                 :mod ""})]
      (t/is (= 1 (count value)))
      (t/is (str/includes? (get (first value) "name") "func2"))
      (t/is (= ":a" (get (first value) "return")))
      (t/is (bencode-roundtrips? value) "the data bencodes cleanly")))
  (t/testing "sayid-query-by-id-data returns the call subtree for an id"
    (let [root-id (-> (sd/ws-deref!) :children first :id name)
          value (captured-value "sayid-query-by-id-data" {:trace-id root-id :mod ""})]
      (t/is (str/includes? (get (first value) "name") "func1"))))
  (t/testing "sayid-query-data runs a query and returns data"
    (let [value (captured-value "sayid-query-data" {:query "()"})]
      (t/is (seq value))
      (t/is (str/includes? (get (first value) "name") "func1")))))

(t/deftest show-traced-renders-the-audit
  (t/testing "sayid-show-traced returns a rendered [text properties] pair"
    (sd/ws-add-trace-ns! ns1)
    (let [value (captured-value "sayid-show-traced" {})
          [text props] value]
      (t/is (= 2 (count value)) "the rendered text/properties pair")
      (t/is (string? text))
      (t/is (str/includes? text "test-ns1") "names the traced namespace")
      (t/is (coll? props)))))
