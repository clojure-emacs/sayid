(ns com.billpiel.mem-tracer.util.tree-query
  (require [clojure.zip :as z]
           [swiss.arrows :refer [-<> -<>>]]))

(def ^:dynamic *get-tags-mz* nil)

(defn right-sib-zips [z] (if-let [r (z/right z)]
                           (lazy-cat [r]
                                     (right-sib-zips r))
                           []))

(defn children-zips [z] (if-let [d (z/down z)]
                          (lazy-cat [d]
                                    (right-sib-zips d))
                          []))

(defn set-zip-children
  [z c]
  (z/edit z #(z/make-node z
                          %
                          c)))

(defn get-tags
  [node pred-map]
  (vec (for [[kw pred] pred-map
             :when (pred node)]
         kw)))

(defn get-tags'
  [node pred-map]
  ((or *get-tags-mz* get-tags)
   node
   pred-map))

#_ (get-tags' {:id 1} {:a #(-> % :id #{1}) :b #(-> % :id #{1})})

(defn merge-children-tag-summary
  [& rest]
  {:paths (->> rest
               (mapcat :paths)
               vec)
   :set (->> rest
             (map :set)
             (apply clojure.set/union))})

(defn insert-tag-into-children-tag-summary
  [tag ch-tags]
  {:paths (->> ch-tags
               :paths
               (mapv #(into [tag] %)))
   :set (-> ch-tags
            :set
            (conj tag))})

(defn get-children-tag-summary*
  [zipr pred-map]
  (let [this-tags (-> zipr
                      z/node
                      (get-tags' pred-map))]
    (insert-tag-into-children-tag-summary this-tags
                                          (if-let [ch-zips (-> zipr
                                                               children-zips
                                                               not-empty)]
                                            (->> ch-zips
                                                 (map #(get-children-tag-summary* %
                                                                                  pred-map))
                                                 (apply merge-children-tag-summary))
                                            (merge-children-tag-summary)))))

(defn get-children-tag-summary
  [zipr pred-map]
  (->> zipr
       children-zips
       (map #(get-children-tag-summary* %
                                        pred-map))
       merge-children-tag-summary))

#_ (ppcp (get-children-tags z1
                            {:a #(-> % :id #{1 2 3 4}) :even #(-<> (do %)
                                                                   :id
                                                                   even?)}))

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

     (def tree {:id 1 :children
                [{:id 2}
                 {:id 3
                  :children [{:id 5
                              :children [{:id 6}]}]}
                 {:id 4
                  :children [{:id 7
                              :children []}
                             {:id 8
                              :children []}
                             ]}]})

     [[[:a :even]]
      [[:a] [] [:even]]
      [[:a :even] []]
      [[:a :even] [:even]]]



     (def ppcp puget.printer/cprint)

     (ppcp tree)

     (def z1 (z/zipper map?
                       #(-> % :children not-empty)
                       #(assoc % :children %2)
                       tree))

     (ppcp (z/root z1))



     (comment))
