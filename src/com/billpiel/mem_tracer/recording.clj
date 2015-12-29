(ns com.billpiel.mem-tracer.recording
  (:require [com.billpiel.mem-tracer.workspace :as ws]
            [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.util.other :as util]))

(def bad-slot-msg "Recording must have a symbol value in :rec-slot. Value was `%s`. Try `save-as!` instead.")
(def unknown-type-msg "Unknown type. `rec` must be a recording, workspace or trace entry. Received a %s.")
(def load-over-unsaved "Current recording is not saved. Use :f as last arg to force, or else `save!` first.")

(defn mk-recording
  [children]
  (-> (trace/mk-entry :id-prefix "rec")
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
      (-> v ws/deref-workspace! :children mk-recording)

      (::trace/entry mv)
      (mk-recording [v])

      :default
      (throw (ex-info (format unknown-type-msg
                              (type v))
                      {::success false
                       ::code :unknown-type})))))


(defn save!
  [rec shelf]
  (let [rec' @rec
        slot (:rec-slot rec')]
    (if (symbol? slot)
      (util/def-ns-var shelf slot rec')
      (throw (Exception. (format
                          bad-slot-msg
                          slot))))
    rec'))

;; rec could be:
;;  recording
;;  workspace
;;  entry (better name?)
(defn save-as!
  [rec shelf slot]
  (doto rec
    (swap! assoc :rec-slot
           (util/qualify-sym shelf slot))
    (save! shelf)))

(defn safe-to-load?
  [rec shelf & [force]]
  (let [rec' @rec]
    (or (= :f force)
        (nil? @rec)
        (some->> @rec
                 :rec-slot
                 (ns-resolve shelf)))))

(defn load!
  [rec shelf slot & [force]]
  (if (safe-to-load? rec shelf force)
    (reset! rec @(ns-resolve shelf slot))
    (throw (Exception. load-over-unsaved))))

(defn coerce&load!
  [rec source shelf & force]
  (if (safe-to-load? rec shelf)
    (let [source' (try (-> source
                           util/atom?->
                           ->recording)
                       (catch Exception ex
                         (if (-> ex ex-data ::code (= :unknown-type))
                           (throw (Exception. (.getMessage ex))) ;; uh... prob get rid of this
                           (throw ex))))]
      (reset! rec source'))
    (throw (Exception. load-over-unsaved))))
