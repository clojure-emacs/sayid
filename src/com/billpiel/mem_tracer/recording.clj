(ns com.billpiel.mem-tracer.recording
  (:require [com.billpiel.mem-tracer.workspace :as ws]
            [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.util.other :as util]
            [com.billpiel.mem-tracer.shelf :as shelf]))

(def bad-slot-msg "Recording must have a symbol value in :rec-slot. Value was `%s`. Try `save-as!` instead.")
(def unknown-type-msg "Unknown type. `rec` must be a recording, workspace or trace tree. Received a %s.")
(def load-over-unsaved "Current recording is not saved. Use :f as last arg to force, or else `save!` first.")

(defn reset-to-nil!
  [rec]
  (reset! rec nil))

(defn mk-recording
  [children]
  (-> (trace/mk-tree :id-prefix "rec")
      (merge {:rec-slot nil
              :children children})
      (vary-meta assoc
                 ::recording
                 true)))

(defn ->recording
  [v]
  (let [mv (meta v)]
    (cond
      (::recording mv)
      v

      (::ws/workspace mv)
      (-> v ws/deref! :children mk-recording)

      (::trace/tree mv)
      (mk-recording [v])

      :default
      (throw (ex-info (format unknown-type-msg
                              (type v))
                      {::success false
                       ::code :unknown-type})))))

(defn save!
  [rec shelf]
  (shelf/save! rec
               shelf
               :rec-slot
               #(format bad-slot-msg %)))

;; rec could be:
;;  recording
;;  workspace
;;  tree
(defn save-as!
  [rec shelf slot]
  (shelf/save-as! rec
                  shelf
                  :rec-slot
                  slot
                  #(format bad-slot-msg %)))

(defn load!
  [rec shelf slot & [force]]
  (shelf/load! rec
               shelf
               :rec-slot
               slot
               load-over-unsaved
               force))

(defn coerce&load!
  [rec source shelf & [force]]
     (shelf/load! rec
                  shelf
                  :rec-slot
                  (try (-> source
                           util/atom?->
                           ->recording)
                       (catch Exception ex
                         (if (-> ex ex-data ::code (= :unknown-type))
                           (throw (Exception. (.getMessage ex))) ;; uh... prob get rid of this
                           (throw ex))))
                  load-over-unsaved
                  force))
