(ns com.billpiel.mem-tracer.query
  (require [clojure.zip :as z]))

(defn entry->seq
  [entry]
  (lazy-cat [entry] (mapcat entry->seq (:children entry))))

(defn query-cat
  [qry-fn entry]
  (->> entry
       entry->seq
       (keep #(when (qry-fn %) %))))

(defn query-tree
  [qry-fn entry]
  (let [e' (update-in entry [:children]
                      (partial mapcat
                               (partial query-tree
                                        qry-fn)))]
    (if (qry-fn entry)
      [e']
      (:children e'))))

(defn mk-query-fn
  [path pred]
  (let [pred' (cond (fn? pred)
                    pred
                    (instance? java.util.regex.Pattern pred)
                    #(->> %
                          str
                          (re-matches pred))
                    :default (partial = pred))]
    (fn [v]
      (-> v
          (get-in path)
          pred'))))

(defn trace->zipper
  [trace]
  (z/zipper map?
            #(-> % :children not-empty)
            #(assoc % :children %2)
            trace))
