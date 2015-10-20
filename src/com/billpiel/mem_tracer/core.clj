(ns com.billpiel.mem-tracer.core
  (:require com.billpiel.mem-tracer.string-output
            [com.billpiel.mem-tracer.trace :as trace]))

(def workspace (atom nil))

(defn default-workspace
  [& {:as m}]
  (merge ^::entry {:id (name (gensym "root"))
                   :depth 0
                   :children (atom [])
                   :traced #{}}
         m))

(defn init-workspace!
  []
  (if (= nil @workspace)
    (compare-and-set! workspace nil (default-workspace))))

(defn reset-workspace!
  []
  (reset! workspace nil))

(defn get-current-workspace!
  []
  (init-workspace!)
  @workspace)

(defn add-trace-ns!
  [ns-sym]
  (init-workspace!)
  (swap! workspace #(update-in % [:traced] conj [:ns ns-sym]))
  (trace/trace-ns* ns-sym @workspace))

(defn remove-trace-ns!
  "Untrace all fns in the given name space."
  [ns-sym]
  (init-workspace!)
  (swap! workspace update-in [:traced] disj [:ns ns-sym])
  (trace/untrace-ns* ns-sym))

(defn enable-all-traces!
  []
  (let [w (get-current-workspace!)]
    (doseq [[type sym] (:traced w)]
      (trace/trace* type sym w))))

(defn disable-all-traces!
  []
  (doseq [t (:traced (get-current-workspace!))]
    (apply trace/untrace* t)))

(defn remove-all-traces! []
  (disable-all-traces!)
  (swap! workspace assoc :traced #{}))

(defn deref-workspace!
  [& [v]]
  (clojure.walk/prewalk #(if (-> %
                                 meta
                                 ::entry)
                           (update-in % [:children] deref)
                           %)
                        (or v (get-current-workspace!))))

(def entry->string com.billpiel.mem-tracer.string-output/entry->string)
