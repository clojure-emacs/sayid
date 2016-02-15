(ns com.billpiel.mem-tracer.query2
  (require [clojure.zip :as z]
           [com.billpiel.mem-tracer.util.other :as util]
           ))

;; === zipper iterators

(defn iter-while-identity
  [f v]
  (->> v
       (iterate f)
       (take-while identity)))

(defn right-sib-zips
  [z]
  (->> z
       (iter-while-identity z/right)
       rest))

(defn left-sib-zips
  [z]
  (->> z
       (iter-while-identity z/left)
       rest))

(defn all-sib-zips [z]
  (->> z
       z/leftmost
       right-sib-zips
       (filter #(not (= % z)))))

(defn ancestor-zips
  [z]
  (->> z
       (iter-while-identity z/up)
       rest))

(defn children-zips [z]
  (->> z
       z/down
       (iter-while-identity z/right)))

(defn descendant-depth-firstish-zips [z]
  (let [kids (children-zips z)]
    (lazy-cat kids
              (mapcat descendant-depth-first-zip
                      kids))))

;; === tags

(defn get-tags
  [node pred-map]
  (vec (for [[kw pred] pred-map
             :when (pred node)]
         kw)))

(defn get-tag-map
  [tree pred-map]
  (reduce (fn [tag-map node]
            (assoc tag-map
                   (:id node)
                   (get-tags node pred-map)))
          {}
          (tree-seq map? :children tree)))

(defn mk-get-tag-fn
  [tag-map]
  (fn [tree]
    (-> tree
        :id
        tag-map)))

;; === zippers

(defn tree->zipper
  [tree]
  (z/zipper map?
            #(-> % :children not-empty)
            #(assoc % :children %2)
            tree))

(defn traverse-assoc-zipper
  [zipr]
  (if (z/end? zipr)
    (-> zipr z/root)
    (recur (-> zipr
               (z/edit assoc :zipper zipr)
               z/next))))

(defn traverse-tree-assoc-zipper
  [tree]
  (-> tree
      tree->zipper
      traverse-assoc-zipper))

;; move this to trace or utils?
(defn traverse-tree
  [tree f]
  (assoc (f tree)
         :children (mapv #(traverse-tree % f)
                         (:children tree))))

(defn traverse-tree-dissoc-zipper
  [tree]
  (traverse-tree tree #(dissoc % :zipper)))

;; === query

(defn query-tree
  [qry-fn tree]
  (let [e' (update-in tree [:children]
                      (partial mapcat
                               (partial query-tree
                                        qry-fn)))]
    (if (qry-fn tree)
      [e']
      (:children e'))))

(defn query-dos
  [tree pred-map pred-final-fn] ;; pred-final-fn takes a node (w/ :zipper) and fn to retrieve tags from a node
  (let [tag-map (get-tag-map tree pred-map)
        get-tag-fn (mk-get-tag-fn tag-map)
        pred-final-fn' #(pred-final-fn % get-tag-fn)]
    (->> tree
         tree->zipper
         traverse-assoc-zipper
         (query-tree pred-final-fn'))))

(defn query-uno
  [tree pred-fn]
  (query-tree pred-fn tree))

(defn query
  ([tree pred-fn] (query-uno tree pred-fn))
  ([tree pred-map pred-final-fn] (query-dos tree pred-map pred-final-fn)))

;; === macro interface


(defn get-some*
  [f v]
  (cond
    (fn? f)
    (f v)

    (set? f)
    (f v)

    :default
    (get v f)))

(defn get-some
  [coll v]
  (loop [coll coll
         v v]
    (if ((some-fn empty? nil?) coll)
      v
      (let [[f & r] coll]
        (when-let [v' (get-some* f v)]
          (recur r v'))))))

(defn eq* [pred v]
  (cond (fn? pred)
        (pred v)

        (set? pred)
        (pred v)

        (instance? java.util.regex.Pattern pred)
        (->> v
             str
             (re-matches pred))

        :default (= pred v)))

(defn mk-query-fn
  [query-coll]
  (let [path (drop-last query-coll)
        pred (last query-coll)]
    (fn [v]
      (->> v
           (get-some path)
           (eq* pred)))))

(defn some-mk-query-fn
  [queries]
  (->> queries
       (map mk-query-fn)
       (apply some-fn)))

(defn relatives-dispatch
  [_ opts _ _]
  (cond
    (= opts '(:r)) :range
    (= opts '(:w)) :simple

    (every? #{:a :s :d}
            opts)
    :simple

    :else (throw (Exception. (format "Unknown query type: '%s'"
                                     opts)))))

(defmulti exec-relatives-query relatives-dispatch)

(defmethod exec-relatives-query :simple
  [tree opts dist pred-vecs]
  (query tree
         (some-mk-query-fn pred-vecs)
))

(defmulti exec-query (fn [_ body] (-> body first type)))

(defmethod exec-query clojure.lang.PersistentVector
  [tree body]
  (query tree
         (some-mk-query-fn body)))

(defmethod exec-query clojure.lang.Symbol
  [tree [syms & r :as body]]
  (let [[fr & rr] r
        [dist pred-vecs] (if (number? fr)
                           [fr rr]
                           [nil r])
        opts (->> syms
                  name
                  seq
                  (map str)
                  (map keyword))]
    (exec-relatives-query tree
                          opts
                          dist
                          pred-vecs)))

(defn q
  [tree & body]
  (exec-query tree body))

;; === helpers

(defn some-zippers
  [zipr-seq node tags-fn pred-fn]
  (->> zipr-seq
       (map z/node)
       (map tags-fn)
       (some pred-fn)))

(defmacro defn-zipper->>
  [name & body]
  `(defn ~name
     [node#]
     (->> node#
          :zipper
          ~@body)))

(defmacro defn-tags-functor
  [name & body]
  `(defn ~name
     [node# tags-fn#]
     (->> node#
          ~@body
          (map z/node)
          (map tags-fn#))))

(defn-zipper->> lazy-ancestor-zipr-seq ancestor-zips)
(defn-tags-functor lazy-ancestor-tag-seq lazy-ancestor-zipr-seq)

(defn-zipper->> lazy-descendant-zipr-seq descendant-depth-firstish-zips)
(defn-tags-functor lazy-descendant-tag-seq lazy-descendant-zipr-seq)

(defn-zipper->> lazy-sibling-zipr-seq all-sib-zips)
(defn-tags-functor lazy-sibling-tag-seq lazy-sibling-zipr-seq)


(defn some-tags
  [tag-set tag-seq]
  (some (partial some
                 tag-set)
        tag-seq))



#_ (do

    (com.billpiel.mem-tracer.util.other/ns-unmap-all *ns*)

(def ttt {:id 1
          :children [{:id 2
                      :children [{:id 4
                                  :children [{:id 5} {:id 6}]}
                                 {:id 7}]}
                     {:id 3}]})

(time (def rrr (mapv traverse-tree-dissoc-zipper
                     (query-dos ttt
                                {:a #(-> % :id #{2})}
                                (fn [node f]
                                  (->> [node f]
                                       (apply lazy-ancestor-tag-seq)
                                       (some-tags #{:a})))))))



(clojure.pprint/pprint rrr)

   ;; traverse tree
   ;; a node provides:
   ;;  its tags
   ;;  its child paths
   ;;  its children's parent
   ;;  its children's siblings

   ;; compile those into a maps like:
   ;;  { :#id#  { :parent [:## :## :##]
   ;;             :children [[:## ##] ... ] ...
   ;;  { :#id# #{...tags} ...}

   ;; traverse tree with tree-query
   ;; pred-fn does this:
   ;;   assembles tag sets
   ;;   attaches to node meta
   ;;   passes to inner pred-fn

   )
