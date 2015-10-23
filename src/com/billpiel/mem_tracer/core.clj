(ns com.billpiel.mem-tracer.core
  (:require com.billpiel.mem-tracer.string-output
            [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.workspace :as ws]))

(def workspace (atom nil))

(defn get-current-workspace!
  []
  (#'ws/init-workspace! workspace)
  @workspace)

(def reset-workspace! (partial #'ws/reset-workspace! workspace))
(def clear-log! (partial #'ws/clear-log! workspace))
(def add-trace-ns! (partial #'ws/add-trace-ns! workspace))
(def remove-trace-ns! (partial #'ws/remove-trace-ns! workspace))
(def enable-all-traces! (partial #'ws/enable-all-traces! workspace))
(def disable-all-traces! (partial #'ws/disable-all-traces! workspace))
(def remove-all-traces! (partial #'ws/remove-all-traces! workspace))
(def deref-workspace! (partial #'ws/deref-workspace! workspace))


(def entry->string com.billpiel.mem-tracer.string-output/entry->string)

(defn print-entry
  [entry]
  (-> entry
      deref-workspace! ;; ??? This can safely be run on a deref'd entry
      com.billpiel.mem-tracer.string-output/print-entry))
