(ns com.billpiel.mem-tracer.util.tree-query
  (require [clojure.zip :as z]
           [swiss.arrows :refer [-<> -<>>]]))

(def ^:dynamic *get-tags-mz* nil)

(defn right-sib-zips [z]
  (if-let [r (z/right z)]
    (lazy-cat [r]
              (right-sib-zips r))
    []))

(defn children-zips [z]
  (if-let [d (z/down z)]
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

(defn insert-tags-into-parent-tag-summary
  [& [tags summary]]
  {:path (if tags
           (-<> summary
                :path
                (or [])
                (into [tags]
                      <>))
           [])
   :set (if tags
          (-<> summary
               :set
               (or #{})
               (apply conj <> tags))
          #{})})

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
  {:paths (-<> ch-tags
               :paths
               not-empty
               (or [[]])
               (mapv #(into [tag] %)
                     <>))
   :set (-<> ch-tags
             :set
             (or #{})
             (apply conj <> tag))})

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

(defn get-children-tag-summary ;; memoize?
  [zipr pred-map]
  (->> zipr
       children-zips
       (map #(get-children-tag-summary* %
                                        pred-map))
       (apply merge-children-tag-summary)))

#_ (ppcp (get-children-tag-summary z1
                                   {:a #(-> % :id #{1 2 3 4}) :even #(-> (do %)
                                                                         :id
                                                                         even?)}))

(defn get-parents-tag-summary ;; memoize?
  [zipr pred-map]
  (if-let [p (z/up zipr)]
    (insert-tags-into-parent-tag-summary (get-tags' (z/node p)
                                                    pred-map)
                                         (get-parents-tag-summary (z/up p)
                                                                  pred-map))
    (insert-tags-into-parent-tag-summary))) ;; tail recursion possible?

(defn tag*
  [zipr pred-map]
  (z/edit zipr (fn [z] (with-meta z
                         {::? {:tags  (get-tags' (z/node zipr) pred-map)
                               :parents (get-parents-tag-summary zipr pred-map)
                               :children (get-children-tag-summary zipr
                                                                   pred-map)}}))))

(defn tag
  [zipr pred-map]
  (binding [*get-tags-mz* (memoize get-tags)]
    (loop [z (-> zipr
                 (tag* pred-map))]
      (let [zn (z/next z)]
        (if-not (z/end? zn)
          (recur (tag* zn pred-map))
          ;; reset zipper
          (z/edit zipr (-> z z/root constantly)))))))

(defn pred-query
  [zipr pred]
  (let [z' (set-zip-children zipr
                             (->> zipr
                                  children-zips
                                  (mapcat #(pred-query %
                                                       pred))
                                  vec))
        n (z/node z')]
    (if (-> n
            meta
            ::?
            pred)
      [n] ;; clear meta ::?
      (z/children z'))))

(defn query
  [zipr pred-map pred-final]
  (-> zipr
      (tag pred-map)
      (pred-query pred-final)))

(defn has-all-tags-fn
  [& tags]
  (fn [node]
    (clojure.set/subset? (set tags)
                         (-> node :tags set))))

(defn has-any-tags-fn
  [& tags]
  (fn [node]
    (some (set tags)
          (:tags node))))


(defn has-child-fn [])

(defn has-descen-fn
  [& tags]
  (fn [node]
    (some (set tags)
          (-> node
              :children
              :set))))


(defn has-parent-fn [])
(defn has-ancest-fn [])
(defn has-closer-ancest-fn [])
(defn has-closer-descen-fn [])
(defn is-between-fn [])
(defn is-strict-between-fn [])

#_ (do
     (def z2 (tag z1
                  {:a #(-> % :id #{1 2 3 4}) :even #(-<> (do %)
                                                         :id
                                                         even?)}))

     (ppcp tree2)
     (println)
     (-> tree2
         meta
         ppcp))

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

     (def qr1 (query z1 {:a #(-> % :id #{6})}
                     #(->> % :children :set (some #{:a}))
                     ))

     (ppcp (z/root z1))



     (comment))
