(ns com.billpiel.mem-tracer.workspace
  (:require [com.billpiel.mem-tracer.trace :as trace]
            [com.billpiel.mem-tracer.util.other :as util]))

(defn default-workspace
  []
  (-> (trace/mk-entry :id-prefix "root")
      (merge {:traced #{}
              :ws-slot nil})
      (vary-meta assoc
                 ::workspace
                 true)))

(defn workspace->entry
  [ws]
  (-> ws
      (dissoc :traced
              :ws-slot)
      (vary-meta dissoc ::workspace)))

(defn init-workspace!
  [ws]
  (if (= nil @ws)
    (compare-and-set! ws nil (default-workspace))
    (throw
     (Exception.
      "Cannot run `init-workspace!` if workspace is not `nil`. Run `reset-workspace!` first."))))

(defn reset-workspace!
  [ws]
  (reset! ws nil))

(defn clear-log!
  [ws]
  (swap! ws assoc :children (atom [])))

(defn add-trace-ns!
  [ws ns-sym]
  (init-workspace! ws)
  (swap! ws #(update-in % [:traced] conj [:ns ns-sym]))
  (trace/trace-ns* ns-sym @ws))

(defn remove-trace-ns!
  "Untrace all fns in the given name space."
  [ws ns-sym]
  (init-workspace! ws)
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

(defn deref-workspace!
  [ws]
  (clojure.walk/prewalk #(if (and (-> %
                                      meta
                                      ::trace/entry)
                                  (-> %
                                      :children
                                      util/atom?))
                           (update-in % [:children] deref)
                           %)
                        (if (util/atom? ws)
                          @ws
                          ws)))

(defn save!
  [ws ws-shelf]
  (let [ws' @ws
        slot (:ws-slot ws')]
    (if (symbol? slot)
      (util/def-ns-var ws-shelf slot ws')
      (throw (Exception. (format "Workspace must have a symbol value in :ws-slot. Value was `%s`. Try `save-as!` instead."
                                 slot))))
    ws'))

(defn save-as!
  [ws ws-shelf slot]
  (swap! ws assoc :ws-slot (util/qualify-sym ws-shelf slot))
  (save! ws ws-shelf))

;; TODO remove traces before unloading a ws???
(defn load!
  [ws ws-shelf slot & [force]]
  (if (or (nil? @ws)
          (some->> @ws
                   :ws-slot
                   (ns-resolve ws-shelf))
          (= :f force))
    (reset! ws @(ns-resolve ws-shelf slot))
    (throw (Exception. "Current workspace is not saved. Use :f as last arg to force, or else `save!` first."))))
