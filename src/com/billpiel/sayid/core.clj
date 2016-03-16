(ns com.billpiel.sayid.core
  (:require com.billpiel.sayid.string-output
            [com.billpiel.sayid.trace :as trace]
            [com.billpiel.sayid.deep-trace :as dtrace]
            [com.billpiel.sayid.workspace :as ws]
            [com.billpiel.sayid.recording :as rec]
            [com.billpiel.sayid.query2 :as q]
            [com.billpiel.sayid.util.find-ns :as find-ns]
            [com.billpiel.sayid.string-output :as so]
            [com.billpiel.sayid.util.other :as util]))

(def workspace (atom nil))
(def recording (atom nil))

(def config (atom {:ws-ns '$ws
                   :rec-ns '$rec}))

;; === Helper functions

(defmacro src-in-meta
  "Takes a `body` form that evaluates to a var. Alters the var's meta
  to include the body source in :source. Useful for functions where
  source is not otherwise available -- ex eval'd outside the context of
  a file.

 Usage:

user> (sd/src-in-meta defn f1 [a] (inc a))
{:arglists ([a]), :line 1, :column 1, :file \"/tmp/form-init5170899558834081664.clj\", :name f1, :ns #object[clojure.lang.Namespace 0x7d351966 \"user\"], :source (defn f1 [a] (inc a))}

user> (-> #'f1 meta :source)
(defn f1 [a] (inc a))
"
  [& body]
  `(util/src-in-meta ~@body))

;; === Workspace functions

(defn- ws-init! [& [quiet]]
  (#'ws/init! workspace quiet))

(defn ws-get-current!
  "Returns the active workspace"
  []
  @(ws-init! :quiet))
(util/defalias w-gc! ws-get-current!)

(defn ws-show-traced
  [& [ws]]
  (:traced (or ws
               (ws-get-current!))))
(util/defalias w-st ws-show-traced)

(defn ws-remove-all-traces!
  "Disables and removes all traces in the active workspace."
  []
  (#'ws/remove-all-traces! workspace)
  (ws-show-traced))
(util/defalias w-rat! ws-remove-all-traces!)

(defn ws-reset!
  "Removes all traces set by active workspace. Resets the active workspace to nil."
  []
  (ws-remove-all-traces!)
  (#'ws/reset-to-nil! workspace))
(util/defalias w-rs! ws-reset!)

(defn ws-clear-log!
  "Clears the log of the active workspace, but preserves traces and other
  properties."
  [] (#'ws/clear-log! workspace))
(util/defalias w-cl! ws-clear-log!)

(defn ws-add-trace-fn!*
  "`fn-sym` is a symbol that references an existing function. Applies an
  enabled trace to said functions. Adds the traces to the active
  workspace trace set."
  [fn-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :fn
                     fn-sym)
  fn-sym)

(defmacro ws-add-trace-fn!
  [fn-sym]
  `(ws-add-trace-fn!* (util/fully-qualify-sym '~fn-sym)))
(util/defalias-macro w-atf! ws-add-trace-fn!)

(defn ws-add-deep-trace-fn!*
  "`fn-sym` is a symbol that references an existing function. Applies an
  enabled trace to said functions. Adds the traces to the active
  workspace trace set."
  [fn-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :deep-fn
                     fn-sym)
  fn-sym)

(defmacro ws-add-deep-trace-fn!
  [fn-sym]
  `(ws-add-deep-trace-fn!* (util/fully-qualify-sym '~fn-sym)))
(util/defalias-macro w-adtf! ws-add-deep-trace-fn!)

(defn ws-add-trace-ns!*
  "`ns-sym` is a symbol that references an existing namespace. Applies an enabled
  trace to all functions in that namespace. Adds the traces to the active workspace trace set."
  [ns-sym]
  (#'ws/add-trace-*! (ws-init! :quiet)
                     :ns
                     ns-sym)
  ns-sym)

(defmacro ws-add-trace-ns!
  "`ns-sym` is a symbol that references an existing namespace. Applies an enabled
  trace to all functions in that namespace. Adds the traces to the active workspace trace set."
  [ns-sym]
  (let [ref-ns *ns*]
    `(mapv ws-add-trace-ns!* (find-ns/search-nses '~ns-sym ~ref-ns))))
(util/defalias-macro w-atn! ws-add-trace-ns!)

(defn ws-remove-trace-ns!
  "`ns-sym` is a symbol that references an existing namespace. Removes all
  traces applied to the namespace."
  [ns-sym] (#'ws/remove-trace-*! (ws-init! :quiet)
                                  :ns
                                  ns-sym))
(util/defalias w-rtn! ws-remove-trace-ns!)

(defn ws-enable-all-traces!
  "Enables any disabled traces in active workspace."
  [] (#'ws/enable-all-traces! workspace))
(util/defalias w-eat! ws-enable-all-traces!)


(defn ws-disable-all-traces!
  "Disables all traces in active workspace. The active workspace trace set will be
  preserved and can be re-enabled."
  [] (#'ws/disable-all-traces! workspace))
(util/defalias w-dat! ws-disable-all-traces!)

(defn ws-cycle-all-traces!
  []
  (ws-disable-all-traces!)
  (ws-enable-all-traces!))
(util/defalias w-cat! ws-cycle-all-traces!)

(defn ws-deref!
  "Returns the value of the active workspace, but with all children
  recursively dereferenced. This workspace value will not receive new
  trace entries."
  [] (#'ws/deep-deref! workspace))
(util/defalias w-drf! ws-deref!)

(defn ws-save!
  "Saves active workspace to the workspace shelf namespace in the pre-specified slot."
  []
  (#'ws/save! workspace (:ws-ns @config)))
(util/defalias w-s! ws-save!)

(defn ws-save-as!
  "Saves active workspace to the workspace shelf namespace in the specified `slot`."
  [slot]
  (#'ws/save-as! workspace
                 (:ws-ns @config)
                 slot)
  true)
(util/defalias w-sa! ws-save-as!)

(defn ws-load!
  "Loads a workspace from the shelf namespace into the active
  position. Will not overwrite an un-saved active workspace unless
  `force` equals :f"
  [slot & [force]]
  (#'ws/load! workspace
              (:ws-ns @config)
              slot
              force)
  true)
(util/defalias w-l! ws-load!)

;; === END Workspace functions

;; === Recording functions

(defn rec-reset!
  "Removes all traces set by active workspace. Resets the active workspace to nil."
  []
  (#'rec/reset-to-nil! recording))
(util/defalias r-rst! rec-reset!)

(defn rec-save!
  "Saves active recording to the recording shelf namespace in the pre-specified slot."
  []
  (#'rec/save! recording (:rec-ns @config))
  true)
(util/defalias r-s! rec-save!)

(defn rec-save-as!
  "Saves active recording to the recording shelf namespace in the specified `slot`."
  [slot]
  (->>  (#'rec/save-as! recording
                        (:rec-ns @config)
                        slot)
        ((juxt :id :rec-slot))
        (apply format "Saved recording with id '%s' to slot '%s'.")))
(util/defalias r-sa! rec-save-as!)

(defn rec-load!
  "Loads a recording from the shelf namespace into the active
  position. Will not overwrite an un-saved active recording unless
  `active` equals :f"
  [slot & [force]]
  (#'rec/load! recording
              (:rec-ns @config)
              slot
              force))
(util/defalias r-l! rec-load!)

(defn rec-load-from!
  "Loads a recording from the provided source. Source may be a workspace"
  [src & [force]]
  (->> (#'rec/coerce&load! recording
                           src
                           (:rec-ns @config)
                           force)
       ((juxt :id :rec-slot))
       (apply format "Loaded recording with id '%s', slot '%s' to active position.")))
(util/defalias r-lf! rec-load-from!)

(defn rec-load-from-ws!
  [& [force]]
  (rec-load-from! (ws-get-current!) force)
  true)
(util/defalias r-lfw! rec-load-from-ws!)

;; === END Recording functions


;; === Query functions

(defmacro qw
  "Queries the trace record of the active workspace."
  [& body] `(q/q (ws-deref!)
                 ~@body))

(defmacro qr
  "Queries the active trace recording."
  [& body] `(q/q @recording
                 ~@body))

(defmacro qt
  "Queries the trace record of the active workspace."
  [tree & body] `(q/q ~tree
                      ~@body))

;; === END Query functions

;; === String Output functions

(def tree->string #'so/tree->string)

(defn get-trees
  [v]
  (let [mk (meta v)]
    (cond
      (sequential? v)
      v

      ((some-fn ::ws/workspace
                ::rec/recording
                ::q/query-result)
       mk)
      (:children v)

      (::trace/tree mk)
      [v]

      (every? #(contains? v %)
              [:children :depth :args :name :return :arg-map :id])
      [v]

      :default
      (throw (Exception. (format "Don't know how to get a tree from this thing. keys=> %s, meta=> %s"
                                 (keys v)
                                 (meta v)))))))

; rec-print ws-print print-trees

(defn print-trees
  [coll]
  (-> coll
      get-trees
      (#'so/print-trees)))
(util/defalias p-t print-trees)

(defn ws-print-unlimited
  [& [ws]]
  (#'so/print-tree-unlimited (or ws
                       (ws-deref!))))
(util/defalias w-pru ws-print-unlimited)

(defn ws-print
  [& [ws & {:keys [max-arg-lines mal max-chars mc]}]]
  (binding [so/*max-chars* (or max-chars
                               mc
                               so/*max-chars*)
            so/*max-arg-lines* (or max-arg-lines
                                   mal
                                   so/*max-arg-lines*)]
    (#'so/print-tree (or ws
                         (ws-deref!)))))
(util/defalias w-pr ws-print)

(defn rec-print
  [& [rec]]
  (#'so/print-tree (or rec
                       @recording)))
(util/defalias r-pr rec-print)

;; === END String Output functions
