(ns com.billpiel.mem-tracer.util.tree-query
  (require [clojure.zip :as z]))

(def ^:dynamic *get-tags-mz* nil)

(defn right-sib-zips [z] (if-let [r (z/right z)]
                           (lazy-cat [r]
                                     (right-sib-zips r))
                           []))

(defn children-zips [z] (if-let [d (z/down z)]
                          (lazy-cat [d]
                                    (right-sib-zips d)) []))

(defn set-zip-children
  [z c]
  (z/edit z #(z/make-node z
                          %
                          c)))

(defn get-tags
  [node pred-map]
  (for [[kw pred] pred-map :when (pred node)]
    kw))

(defn get-tags'
  [node pred-map]
  ((or *get-tags-mz* get-tags)
   node
   pred-map))

(defn get-children-tags
  [zipr]
  (let [kids (children-zips zipr)]
    (apply concat (for [child-zip kids
                        desc-tags (get-children-tags child-zip)]
                    (map (fn [tag-trail] (conj tag-trail (get-tags' child-zip))))))))

(defn tag*
  [zipr pred-map & {:keys [parent-tags]}]
  (let [this-tags (get-tags' (z/node zipr) pred-map)
        parent-tags' (conj parent-tags)
        children-tags (get-children-tags zipr)
])
  (z/edit zipr (fn [z] (with-meta z
                         {:tags (get-tags' (z/node z) pred-map)
                          :parent-tags []
                          :parent-tag-set #{}
                          :children []
                          :children-set #{}}))))

(defn tag
  [zipr pred-map]
  (binding [*get-tags-mz* (memoize get-tags)]
    (tag* zipr
          pred-map)))

#_ (do


     (def z1 (z/zipper map? :children #(assoc % :children %2)
                       {:a 1 :children [{:a 2} {:a 3 :children []} {:a 4}]}))



     (comment))
