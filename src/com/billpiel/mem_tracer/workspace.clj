(ns com.billpiel.mem-tracer.workspace
  (:require [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.util.other :as util]
            [com.billpiel.mem-tracer.shelf :as shelf]
            [com.billpiel.mem-tracer.util.walk18 :as walk-1.8]))

(defn default-workspace
  []
  (-> (trace/mk-tree :id-prefix "root")
      (merge {:traced #{}
              :ws-slot nil})
      (vary-meta assoc
                 ::workspace
                 true)))

(defn workspace->tree
  [ws]
  (-> ws
      (dissoc :traced
              :ws-slot)
      (vary-meta dissoc ::workspace)))

(defn init!
  [ws & [quiet]]
  (when-not (or (compare-and-set! ws nil (default-workspace))
                (#{:quiet} quiet))
    (throw
     (Exception.
      "Cannot run `ws-init!` if workspace is not `nil`. Run `ws-reset!` first or pass :quiet as second arg.")))
  ws)

(defn reset-to-nil!
  [ws]
  (reset! ws nil))

(defn clear-log!
  [ws]
  (swap! ws assoc :children (atom [])))

(defn add-trace-ns!
  [ws ns-sym]
  (swap! ws #(update-in % [:traced] conj [:ns ns-sym]))
  (trace/trace-ns* ns-sym @ws))

(defn remove-trace-ns!
  "Untrace all fns in the given name space."
  [ws ns-sym]
  (swap! ws update-in [:traced] disj [:ns ns-sym])
  (trace/untrace-ns* ns-sym))

(defn enable-all-traces!
  [ws]
  (let [w @ws]
    (doseq [[type sym] (:traced w)]
      (trace/trace* type sym w))))

(defn disable-all-traces!
  [ws]
  (doseq [t (:traced @ws)]
    (apply trace/untrace* t)))

(defn remove-all-traces!
  [ws]
  (disable-all-traces! ws)
  (swap! ws assoc :traced #{}))

#_ (defn deref!
     [ws]
     (walk-1.8/prewalk #(if (and (-> %
                                     meta
                                     ::trace/tree)
                                 (-> %
                                     :children
                                     util/atom?))
                          (do (-> % :arg-map force)
                              (update-in % [:children] deref))
                          %)
                       (util/atom?-> ws)))

(defn deep-deref!
  [tree]
  (let [dr-kids (-> tree :children util/atom?->)
        forced-arg-map (-> tree :arg-map force)
        kids (mapv deep-deref! dr-kids)]
    (assoc tree
           :children kids
           :arg-map forced-arg-map)))

(defn save!
  [ws ws-shelf]
  (shelf/save! ws
               ws-shelf
               :ws-slot
               #(format "Workspace must have a symbol value in :ws-slot. Value was `%s`. Try `save-as!` instead." %)))

(defn save-as!
  [ws ws-shelf slot]
  (shelf/save-as! ws
                  ws-shelf
                  :ws-slot
                  slot
                  #(format "Workspace must have a symbol value in :ws-slot. Value was `%s`. Try `save-as!` instead." %)))

;; TODO remove traces before unloading a ws???
(defn load!
  [ws ws-shelf slot & [force]]
  (shelf/load! ws
               ws-shelf
               :ws-slot
               slot
               "Current workspace is not saved. Use :f as last arg to force, or else `save!` first."
               force))
