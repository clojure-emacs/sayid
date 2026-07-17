(ns sayid.nrepl-middleware-test
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
            [sayid.nrepl-middleware :as mw]
            [sayid.core :as sd]
            [sayid.trace :as sdt]
            [sayid.test-utils :as t-utils]
            [sayid.test-ns1 :as ns1]
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
  (sdt/untrace-ns* 'sayid.test-ns1)
  (with-out-str (sd/ws-reset!))
  ;; Deterministic clock and ids, exactly like core-test and public-api-test.
  (with-redefs [sdt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (f)
    (sdt/untrace-ns* 'sayid.test-ns1)))

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

(t/deftest node->data-handles-inner-trace-nodes
  (t/testing "an inner node exposes its form and keeps its return"
    ;; Inner-trace nodes always carry a :throw key, nil unless they actually
    ;; threw; the return must survive that.
    (let [data (mw/node->data
                {:id :9 :name 'apply :depth 2
                 :form '(apply + (map inc xs))
                 :throw nil :return 9
                 :args [] :arg-map {} :children []})]
      (t/is (= "(apply + (map inc xs))" (get data "form"))
            "the source expression is exposed as the form")
      (t/is (= "9" (get data "return")))
      (t/is (not (contains? data "throw"))
            "a nil :throw must not be reported as a throw"))))

(t/deftest node->data-bounds-runaway-values
  (t/testing "an infinite lazy seq is serialized to a bounded string, not a hang"
    (let [data (mw/node->data {:id :1 :name 'f :return (range) :children []})
          ret (get data "return")]
      (t/is (string? ret))
      (t/is (str/includes? ret "...") "the value is marked as truncated")
      (t/is (< (count ret) 1000) "and its printed form is bounded"))))

(t/deftest rendered-query-by-fn-still-returns-the-trio
  (t/testing "splitting query-building out of sayid-query-by-fn keeps it rendering"
    (sd/ws-add-trace-ns! ns1)
    (ns1/func1 :a)
    (let [value (captured-value "sayid-query-by-fn"
                                {:fn-name "sayid.test-ns1/func2"
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
                                {:fn-name "sayid.test-ns1/func2"
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
      (t/is (str/includes? (get (first value) "name") "func1"))))
  (t/testing "sayid-query-form-at-point-data matches calls by source position"
    (let [{:keys [file line]} (meta #'ns1/func1)
          value (captured-value "sayid-query-form-at-point-data"
                                {:file file :line line})]
      (t/is (= 1 (count value)))
      (t/is (str/includes? (get (first value) "name") "func1"))
      (t/is (bencode-roundtrips? value) "the data bencodes cleanly"))))

;;; --- Trace-at-point outcomes -------------------------------------------------

(def ^:private point-source
  "A tiny stand-in for a buffer's source: `func1' sits on line 2."
  "(ns sayid.test-ns1)\n(defn func1 [arg1] arg1)\n")

(defn- trace-at-point
  "Drive sayid-trace-fn-at-point with ACTION on `func1' in `point-source'."
  [action]
  (captured-value "sayid-trace-fn-at-point"
                  {:action action :file "fake.clj" :line 2 :column 8
                   :source point-source}))

(t/deftest trace-fn-at-point-reports-a-fresh-trace
  (let [value (trace-at-point "add-outer")]
    (t/is (= "sayid.test-ns1/func1" (get value "sym")))
    (t/is (= 0 (get value "was-traced")))
    (t/is (contains? (:fn (:traced (sd/ws-get-active!)))
                     'sayid.test-ns1/func1)
          "the outer trace was applied")))

(t/deftest trace-fn-at-point-reports-an-existing-trace
  (trace-at-point "add-outer")
  (let [value (trace-at-point "add-inner")]
    (t/is (= 1 (get value "was-traced")))
    (t/is (contains? (:inner-fn (:traced (sd/ws-get-active!)))
                     'sayid.test-ns1/func1)
          "the trace was switched to inner")))

(t/deftest trace-fn-at-point-skips-managing-an-absent-trace
  (let [value (trace-at-point "enable")]
    (t/is (= 0 (get value "was-traced")))
    (t/is (empty? (:fn (:traced (sd/ws-get-active!))))
          "enable on an untraced fn applies nothing")))

(t/deftest trace-fn-at-point-reports-nothing-at-point
  (let [value (captured-value "sayid-trace-fn-at-point"
                              {:action "add-outer" :file "fake.clj"
                               :line 2 :column 2
                               :source "(ns sayid.test-ns1)\n(nonexistent-fn-xyz 1)\n"})]
    (t/is (empty? value) "no resolvable function replies with an empty map")))

(t/deftest show-traced-renders-the-audit
  (t/testing "sayid-show-traced returns a rendered [text properties] pair"
    (sd/ws-add-trace-ns! ns1)
    (let [value (captured-value "sayid-show-traced" {})
          [text props] value]
      (t/is (= 2 (count value)) "the rendered text/properties pair")
      (t/is (string? text))
      (t/is (str/includes? text "test-ns1") "names the traced namespace")
      (t/is (coll? props)))))

(t/deftest audit->data-groups-fns-by-namespace
  (t/testing "audit->data merges ns- and individually-traced fns into ns groups"
    (let [audit {:ns {'my.ns {'foo {:ns "my.ns" :name 'foo :file "my/ns.clj"
                                    :line 3 :trace-type :fn}}}
                 :fn {'my.ns {'bar {:ns "my.ns" :name 'bar :file "my/ns.clj"
                                    :line 7 :trace-type :inner-fn}}}}
          data (mw/audit->data audit)]
      (t/is (= 1 (count data)) "one namespace group")
      (let [group (first data)]
        (t/is (= "my.ns" (get group "ns")))
        (t/is (= #{"foo" "bar"} (set (map #(get % "name") (get group "fns"))))
              "both the ns- and fn-traced functions appear under the namespace")
        (let [foo (first (filter #(= "foo" (get % "name")) (get group "fns")))]
          (t/is (= "my/ns.clj" (get foo "file")))
          (t/is (= 3 (get foo "line")))
          (t/is (= "fn" (get foo "trace-type"))))))))

(t/deftest show-traced-data-returns-namespace-groups
  (t/testing "the data op groups the traced fns by namespace"
    (sd/ws-add-trace-ns! ns1)
    (let [value (captured-value "sayid-show-traced-data" {})
          group (first value)]
      (t/is (= "sayid.test-ns1" (get group "ns")))
      (t/is (some #(= "func1" (get % "name")) (get group "fns"))
            "func1 is listed under its namespace"))))
