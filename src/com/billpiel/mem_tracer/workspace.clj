(ns com.billpiel.mem-tracer.workspace
  (:require [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.util.other :as util]
            [com.billpiel.mem-tracer.shelf :as shelf]))

(def default-traced {:ns #{}
                       :fn #{}
                       :deep-fn #{}})

(defn default-workspace
  []
  (-> (trace/mk-tree :id-prefix "root")
      (merge {:traced default-traced
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

(defn apply-trace-set-restrictions!
  [ws]
  (if-let [dups (->> @ws
                     :traced
                     ((juxt :fn :deep-fn))
                     (apply clojure.set/union)
                     not-empty)]
    (doseq [d dups]
      (remove-trace-*! ws :fn d))))

(defn add-trace-*!
  [ws type sym]
  (swap! ws (fn [ws'] (-> ws'
                          (update-in [:traced type]
                                     conj sym))))
  (apply-trace-set-restrictions! ws)
  (trace/trace* type sym @ws))

(defn remove-trace-*!
  "Untrace all fns in the given name space."
  [ws type sym]
  (swap! ws update-in
         [:traced type]
         disj sym)
  (trace/untrace* :ns sym))

(defn enable-all-traces!
  [ws]
  (let [w @ws
        f (fn [type] (doseq [sym (get-in w [:traced type])]
                       (trace/trace* type sym w)))]
    (doall (map f [:deep-fn
                   :fn
                   :ns]))
    true))

(defn disable-all-traces!
  [ws]
  (doseq [t (-> @ws
                :traced
                util/flatten-map-kv-pairs)]
    (apply trace/untrace* t)))

(defn remove-all-traces!
  [ws]
  (disable-all-traces! ws)
  (swap! ws assoc :traced default-traced))

(defn deep-deref!
  [tree]
  (if-let [tree' (util/atom?-> tree)]
    (let [dr-kids (-> tree' :children util/atom?->)
          forced-arg-map (-> tree' :arg-map force)
          kids (mapv deep-deref! dr-kids)]
      (assoc tree'
             :children kids
             :arg-map forced-arg-map))))

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
