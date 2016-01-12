(ns com.billpiel.mem-tracer.core
  (:require com.billpiel.mem-tracer.string-output
            [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.workspace :as ws]
            [com.billpiel.mem-tracer.recording :as rec]
            [com.billpiel.mem-tracer.query :as q]
            [com.billpiel.mem-tracer.util.find-ns :as find-ns]))

(def workspace (atom nil))
(def recording (atom nil))

(def config (atom {:ws-ns '$ws
                   :rec-ns '$rec}))

;; === Workspace functions


(defn- init-workspace! [& [quiet]]
  (#'ws/init-workspace! workspace quiet))

(defn get-current-workspace! []
  @(init-workspace! :quiet))

(defn remove-all-traces!
"Disables and removes all traces in the active workspace."
[] (#'ws/remove-all-traces! workspace))

(defn reset-workspace!
  "Removes all traces set by active workspace. Resets the active workspace to nil."
  []
  (remove-all-traces!)
  (#'ws/reset-workspace! workspace))

(defn clear-log!
"Clears the log of the active workspace, but preserves traces and other
  properties."
[] (#'ws/clear-log! workspace))

(defn add-trace-ns!*
  "`ns-sym` is a symbol that references an existing namespace. Applies an enabled
  trace to all functions in that namespace. Adds the traces to the active workspace trace set."
  [ns-sym]
  (#'ws/add-trace-ns! (init-workspace! :quiet)
                      ns-sym)
  ns-sym)

(defmacro add-trace-ns!
  [ns-sym]
  (let [ref-ns *ns*]
    `(mapv add-trace-ns!* (find-ns/search-nses '~ns-sym ~ref-ns))))

(defn remove-trace-ns!
  "`ns-sym` is a symbol that references an existing namespace. Removes all
  traces applied to the namespace."
  [ns-sym] (#'ws/remove-trace-ns! (init-workspace! :quiet)
                                  ns-sym))

(defn enable-all-traces!
"Enables any disabled traces in active workspace."
[] (#'ws/enable-all-traces! workspace))

(defn disable-all-traces!
"Disables all traces in active workspace. The active workspace trace set will be
  preserved and can be re-enabled."
[] (#'ws/disable-all-traces! workspace))

(defn deref-workspace!
"Returns the value of the active workspace, but with all children
  recursively dereferenced. This workspace value will not receive new
  trace entries."
[] (#'ws/deref-workspace! workspace))

(defn ws-save!
  "Saves active workspace to the workspace shelf namespace in the pre-specified slot."
  []
  (#'ws/save! workspace (:ws-ns @config)))

(defn ws-save-as!
  "Saves active workspace to the workspace shelf namespace in the specified `slot`."
  [slot]
  (#'ws/save-as! workspace
                 (:ws-ns @config)
                 slot))

(defn ws-load!
  "Loads a workspace from the shelf namespace into the active
  position. Will not overwrite an un-saved active workspace unless
  `force` equals :f"
  [slot & [force]]
  (#'ws/load! workspace
              (:ws-ns @config)
              slot
              force))


;; === END Workspace functions

;; === Recording functions

(defn rec-save!
  "Saves active recording to the recording shelf namespace in the pre-specified slot."
  []
  (#'rec/save! recording (:rec-ns @config)))

(defn rec-save-as!
  "Saves active recording to the recording shelf namespace in the specified `slot`."
  [slot]
  (#'rec/save-as! recording
                 (:rec-ns @config)
                 slot))

(defn rec-load!
  "Loads a recording from the shelf namespace into the active
  position. Will not overwrite an un-saved active recording unless
  `active` equals :f"
  [slot & [force]]
  (#'rec/load! recording
              (:rec-ns @config)
              slot
              force))

(defn rec-load-from!
"Loads a recording from the provided source. Source may be a work"
  [src & [force]]
  (#'rec/coerce&load! recording
                      src
                      (:rec-ns @config)
                      force))

(defn rec-load-from-ws!
  [& [force]]
  (rec-load-from! (get-current-workspace!) force))

;; === END Recording functions


;; === Query functions

(defmacro qw
"Queries the trace record of the active workspace."
 [& body] `(q/q (q/trace->zipper (deref-workspace!))
                           ~@body))

(defmacro qr
"Queries the active trace recording."
 [& body] `(q/q (q/trace->zipper @recording)
                           ~@body))

;; === END Query functions


(def tree->string #'com.billpiel.mem-tracer.string-output/tree->string)

(defn print-tree
  [tree]
  (-> tree
      (#'ws/deref-workspace!) ;; ??? This can safely be run on a deref'd tree
      (#'com.billpiel.mem-tracer.string-output/print-tree)))

(defn print-ws
  []
  (#'com.billpiel.mem-tracer.string-output/print-tree (deref-workspace!)))
