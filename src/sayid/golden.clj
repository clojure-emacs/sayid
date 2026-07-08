(ns sayid.golden
  "Golden-trace testing: assert that a run's *execution* matches a recorded
  baseline, not just its final output.

  Trace some code, run it, and compare the recorded call tree - functions called,
  arguments, return values, nesting, and (with inner tracing) the intermediate
  expression values - against a stored golden file.  When the trace changes, the
  test fails with a diff; when the change is intentional, regenerate the golden.

  This is regression testing at the level of *how* your code ran, which a plain
  return-value assertion can't see and neither `tools.trace` nor a stepping
  debugger offers.

  Typical use in `clojure.test`:

      (require '[sayid.core :as sd] '[sayid.golden :as gold])

      (deftest orders-golden
        (sd/ws-reset!)
        (sd/ws-add-trace-ns! my.orders)
        (my.orders/place-order sample-order)
        (is (gold/matches-golden? \"place-order\")))

  The first run writes `test/golden/place-order.edn` (review and commit it);
  later runs compare against it.  Regenerate goldens by binding `*update*` true or
  setting the `SAYID_GOLDEN_UPDATE` environment variable."
  (:require [sayid.core :as sd]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.pprint :as pp]))

(def ^:dynamic *golden-dir*
  "Directory the golden `.edn` files live in.  Defaults to `test/golden`."
  "test/golden")

(def ^:dynamic *update*
  "When true, `check-golden` (re)writes the golden instead of comparing.  Defaults
  to whether the `SAYID_GOLDEN_UPDATE` environment variable is set."
  (some? (System/getenv "SAYID_GOLDEN_UPDATE")))

;;; ---- normalization ------------------------------------------------------

(defn- pr-val
  "Print a captured value deterministically and boundedly, so a fat or lazy value
  can't bloat or hang the golden."
  [v]
  (binding [*print-length* 100
            *print-level* 10]
    (pr-str v)))

(defn- drop-nils [m]
  (into {} (remove (comp nil? val) m)))

(defn normalize-node
  "Project a recorded node onto the stable, comparable fields: the function or
  expression, its arguments and return-or-throw as printed values, inner-trace
  tags, and children.  Drops everything volatile - ids, paths, timings, source
  metadata, and the throw's stacktrace."
  [node]
  (drop-nils
   {:name (some-> (:name node) str)
    :form (some-> (:form node) pr-str)
    :args (when (contains? node :args) (mapv pr-val (:args node)))
    :return (when (contains? node :return) (pr-val (:return node)))
    :throw (when-let [th (:throw node)]
             {:type (some-> (get-in th [:via 0 :type]) pr-str)
              :message (:cause th)})
    :inner-tags (:inner-tags node)
    :children (when (seq (:children node))
                (mapv normalize-node (:children node)))}))

(defn golden-trace
  "The normalized golden trace of the active workspace (or workspace W): a vector
  of the recorded root call trees."
  [& [w]]
  (mapv normalize-node (:children (if w (sd/ws-deref! w) (sd/ws-deref!)))))

;;; ---- storage ------------------------------------------------------------

(defn- sort-nested
  "Recursively replace maps with sorted-maps so the written EDN has stable key
  order and diffs cleanly."
  [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [k (sort-nested v)])) x)
    (vector? x) (mapv sort-nested x)
    :else x))

(defn- golden-file ^java.io.File [name]
  (io/file *golden-dir* (str name ".edn")))

(defn read-golden
  "Read the stored golden NAME, or nil if it doesn't exist yet."
  [name]
  (let [f (golden-file name)]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(defn write-golden!
  "Write DATA as the golden NAME, creating the directory if needed.  Returns the
  file."
  [name data]
  (let [f (golden-file name)]
    (io/make-parents f)
    (spit f (with-out-str
              (binding [*print-length* nil *print-level* nil]
                (pp/pprint (sort-nested data)))))
    f))

;;; ---- comparison ---------------------------------------------------------

(defn check-golden
  "Compare DATA against the stored golden NAME.  Returns a map whose `:status` is
  one of `:created` (no golden existed - wrote it), `:updated` (`*update*` was on -
  rewrote it), `:match`, or `:mismatch` (with a `:diff` of
  [in-golden-only in-run-only])."
  [name data]
  (let [existing (read-golden name)]
    (cond
      (nil? existing)     (do (write-golden! name data) {:status :created})
      *update*            (do (write-golden! name data) {:status :updated})
      (= existing data)   {:status :match}
      :else               {:status :mismatch
                           :diff (vec (take 2 (data/diff existing data)))})))

(defn matches-golden?
  "Assert the active workspace's normalized trace matches golden NAME.  Returns
  true on a match (or when the golden was just created/updated, printing a notice);
  on a mismatch prints the diff and returns false, so it reads naturally inside
  `(is (matches-golden? ...))`."
  [name & [w]]
  (let [{:keys [status diff]} (check-golden name (golden-trace w))]
    (case status
      :match true
      :created (do (println "sayid golden:" (str (golden-file name))
                            "created - review and commit it.")
                   true)
      :updated (do (println "sayid golden:" (str (golden-file name)) "updated.")
                   true)
      :mismatch (do (println "sayid golden mismatch for" (pr-str name) "-")
                    (println "  only in golden:" (pr-str (first diff)))
                    (println "  only in run:   " (pr-str (second diff)))
                    false))))
