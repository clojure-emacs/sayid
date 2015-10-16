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

(defn add-trace-ns
  [ns-sym]
  (compare-and-set! workspace nil (default-workspace))
  (swap! workspace #(update-in % [:traced] conj [:ns ns-sym]))
  (trace/trace-ns* ns-sym workspace))

(defn remove-trace-ns
  "Untrace all fns in the given name space."
  [ns-sym]
  (swap! workspace update-in [:traced] #(remove #{[:ns ns-sym]} %))
  (trace/untrace-ns* ns-sym workspace))

(defn enable-all-traces
  []
  (let [w @workspace]
    (doseq [[type sym] (:traced w)]
      (apply trace/trace* type sym w))))

(defn disable-all-traces
  []
  (doseq [t (:traced @workspace)]
    (apply trace/untrace* t)))

(defn remove-all-traces []
  (disable-all-traces)
  (swap! workspace assoc :traced []))

(defn deref-children
  [v]
  (clojure.walk/prewalk #(if (-> %
                                 meta
                                 ::entry)
                           (update-in % [:children] deref)
                           %)
                        v))

(def entry->string com.billpiel.mem-tracer.string-output/entry->string)
