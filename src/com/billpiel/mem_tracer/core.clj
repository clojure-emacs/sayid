(ns com.billpiel.mem-tracer.core
  (:require com.billpiel.mem-tracer.string-output
            [com.billpiel.mem-tracer.trace :as trace]))


(def workspace (atom nil))



(defn default-workspace
  [& {:as m}]
  (merge ^::entry {:id (name (gensym "root"))
                   :depth 0
                   :children (atom [])
                   :traced []}
         m))

(defn default-root
  [& {:as m}]
  (merge ^::entry {:id (name (gensym "root"))
                   :depth 0
                   :children (atom [])}
         m))

(defmacro trace-ns
  "Trace all fns in the given name space."
  [ns & [root]]
  `(let [root# (or ~root (default-root))]
     (trace/trace-ns* ~ns root#)
     root#))

(defmacro untrace-ns
  "Untrace all fns in the given name space."
  [ns]
  `(trace/untrace-ns* ~ns))

(defn deref-children
  [v]
  (clojure.walk/prewalk #(if (-> %
                                 meta
                                 ::entry)
                           (update-in % [:children] deref)
                           %)
                        v))

(def entry->string com.billpiel.mem-tracer.string-output/entry->string)
