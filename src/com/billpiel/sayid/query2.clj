(ns com.billpiel.sayid.query2
  (require [clojure.zip :as z]
           [com.billpiel.sayid.util.other :as util]
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
  (let [lefty (z/leftmost z)
        zips (lazy-cat [lefty] (right-sib-zips lefty))
        not-z #(not (= % z))]
    (filter not-z zips)))

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
              (mapcat descendant-depth-firstish-zips
                      kids))))

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

(defmacro defn-tag-seq-fnr->>
  [name args & body]
  `(defn ~name
     ~(conj args 'tag-seq-functor)
     (fn [~'node ~'tag-fn]
       (->> (~'tag-seq-functor ~'node ~'tag-fn)
            ~@body))))

(defn-tag-seq-fnr->> mk-some-tags-in-seq-fn [tag-set] (some-tags tag-set))
(defn-tag-seq-fnr->> take-lazy-tag-seq-functor [n] (take n))
(defn-tag-seq-fnr->> prepend-node-to-lazy-tag-seq-functor [] (lazy-cat [(tag-fn node)]))

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
  (if-not (nil? tree)
    (let [e' (update-in tree [:children]
                        (partial mapcat
                                 (partial query-tree
                                          qry-fn)))]
      (if (qry-fn tree)
        [e']
        (:children e')))
    []))

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
      (try
        (->> v
             (get-some path)
             (eq* pred))
        (catch Exception ex
          nil)))))

(defn some-mk-query-fn
  [queries]
  (->> queries
       (map mk-query-fn)
       (apply some-fn)))

(defn some-fn-2
  [& preds]
  (fn [node tag-fn]
    (loop [[f & r] preds]
      (or (f node tag-fn)
          (when (not-empty r)
            (recur r))))))

(defn every-pred-2
  [& preds]
  (fn [node tag-fn]
    (loop [[f & r] preds]
      (and (f node tag-fn)
           (if (not-empty r)
             (recur r)
             true)))))

#_ (defn mk-relative-final-qry-fn
     [opts tag-set & [dist]]
     (let [opts' (util/obj-pred-action-else opts
                                            (partial some #{:w})
                                            :t [:a :s :d])
           f' (fn [tag-seq-fnr]
                (let [taker (if dist
                              (partial take-lazy-tag-seq-functor dist)
                              identity)
                      stis (->> tag-seq-fnr
                                taker
                                prepend-node-to-lazy-tag-seq-functor
                                (mk-some-tags-in-seq-fn tag-set))]
                  stis))
           rel-seq-map {:a lazy-descendant-tag-seq
                        :s lazy-sibling-tag-seq
                        :d lazy-ancestor-tag-seq}]
       (->> opts'
            (keep rel-seq-map)
            (map f')
            (apply some-fn-2))))

(defn children-zips [zipr]
  (some->> zipr
           z/down
           (iter-while-identity z/right)))

(defn children-zips-by-generation [zipr]
  (if (not-empty zipr)
    (let [zipr' (if (some-> zipr
                            meta
                            :zip/make-node)
                  [zipr]
                  zipr)
          kids (mapcat children-zips zipr')]
      (concat (if (not-empty kids)
                [kids]
                [])
              (children-zips-by-generation kids)))
    nil))

(defn mk-lazy-descendant-tag-seq
  [node tag-fn dist]
  (def n' node)
  (def tag-fn' tag-fn)
  (def d' dist)
  (let [generations-seq (->> node
                             :zipper
                             children-zips-by-generation)
        g-seq (if dist
                (take dist generations-seq)
                generations-seq)]
    (->> g-seq
         (apply concat)
         (map z/node)
         (mapcat tag-fn))))


(defn mk-lazy-sibling-tag-seq [])
(defn mk-lazy-ancestor-tag-seq [])

(defn mk-relative-final-qry-fn
  [opts tag-set & [dist]]
  (fn [node tag-fn]
    (or (some tag-set (tag-fn node))
        (let [opts' (if (some #{:w} opts)
                      [:a :s :d]
                      opts)
              rel-seq-map {:a mk-lazy-descendant-tag-seq
                           :s mk-lazy-sibling-tag-seq
                           :d mk-lazy-ancestor-tag-seq}
              tag-seq-coll (->> opts'
                                (keep rel-seq-map)
                                (mapv #(% node tag-fn dist)))]
          (some (partial some tag-set)
                tag-seq-coll)))))

(defn parse-to-kw-chars
  [s]
  (->> s
       name
       seq
       (map str)
       (map keyword)))

(defn query-dispatch-decider
  [_ body]
  (let [[f & r] body]
    (if (-> f
            type
            (= clojure.lang.PersistentVector))
      :simple
      (let [kws (parse-to-kw-chars f)]
        (if (every? #{:a :s :d :w :r}
                    kws)
          (condp some kws
            #{:a :s :d :w} :relative
            #{:r} :range)
          (throw (Exception. (format "Invalid query type '%s'" kws))))))))

(defmulti exec-query query-dispatch-decider)

(defmethod exec-query :simple
  [tree body]
  (query tree
         (some-mk-query-fn body)))

(defmethod exec-query :relative
  [tree [syms & r :as body]]
  (let [[fr & rr] r
        [dist pred-vecs] (if (number? fr)
                           [fr rr]
                           [nil r])
        opts (parse-to-kw-chars syms)
        qry-final (mk-relative-final-qry-fn opts
                                            #{:a}
                                            dist)]
    (query tree
           {:a (some-mk-query-fn pred-vecs)}
           qry-final)))

(defmethod exec-query :range
  [tree [_ ancestor descendant]]
  (query tree
         {:a (mk-query-fn ancestor)
          :d (mk-query-fn descendant)}
         (every-pred-2 (mk-relative-final-qry-fn [:d] #{:a})
                       (mk-relative-final-qry-fn [:a] #{:d}))))

(defn mk-query-result-root
  [tree]
  (vary-meta (if (-> tree
                     meta
                     :trace-root)
               tree
               {:id (-> "query-result" gensym keyword)
                :children []})
             assoc
             ::query-result true
             :trace-root true))

(defn q
     [tree & body]
     (let [parent (mk-query-result-root tree)
           [r1 :as result] (vec (exec-query tree
                                            body))]
       (if (= (:id r1) (:id parent))
         r1
         (assoc parent
                :children result))))

;; ========================

(defn get-pos
  [v]
  (-> (or (:src-pos v)
          (:meta v))
      (select-keys [:line :column :file :end-line :end-column])
      (assoc :ids #{(:id v)})))

(defn start-dist
  [pos-line {:keys [line end-line]}]
  (when (<= (or end-line line) pos-line)
    (- pos-line (or end-line line))))

(defn inside-width
  [pos-line {:keys [line end-line]}]
  (when (and end-line
             (<= line pos-line end-line))
    (- end-line line)))

(defn compare-metric
  [better worse]
  (cond
    (= nil better worse) nil
    (not better) false
    (not worse) true
    :else (< better worse)))

(defn merge-em
  [a b]
  (assoc a
         :ids (->> [a b]
                   (map :ids)
                   (apply clojure.set/union))))

(def init-best {:ids #{}
                :line -1
                :end-line nil})

(defn compare-thing
  [line best next]
  (let [best (or best init-best)
        inside-width-best (inside-width line best)
        inside-width-next (inside-width line next)
        inside-width-best-better (compare-metric inside-width-best inside-width-next)
        inside-width-both-nil (= inside-width-best inside-width-next nil)
        inside-width-equal (= inside-width-best inside-width-next)
        start-dist-best (start-dist line best)
        start-dist-next (start-dist line next)
        start-dist-best-better (compare-metric start-dist-best start-dist-next)
        start-dist-both-nil (= start-dist-best start-dist-next nil)
        start-dist-equal (= start-dist-best start-dist-next)]
    (cond
      inside-width-best-better best

      (and (-> inside-width-best nil? not)
           inside-width-equal)
      (merge-em best next)

      (false? inside-width-best-better) next

      start-dist-best-better best
      start-dist-both-nil best
      start-dist-equal (merge-em best next)
      (false? start-dist-best-better) next)))

(defn file-paths-match?
  [test path]
  (let [test' (clojure.string/replace test #"^file:" "")
        path' (clojure.string/replace path #"^file:" "")]
    (.endsWith test' path')))

(defn get-best-match-in-tree-seq
  [ts file line]
  (->> ts
       (map get-pos)
       (filter (fn [v]
                 (when-let [f (:file v)]
                   (file-paths-match? file f))))
       (reduce (partial compare-thing
                        line)
               init-best)))

(defn get-ids-from-file-pos
  [tree file line]
  (util/$- ->> tree
           (tree-seq map? :children)
           (get-best-match-in-tree-seq $ file line)
           :ids))

(defn compare-positions-against-line-range
  [start-line line best next]
  (if (<= start-line
          (:line next))
    (compare-thing line
                   best
                   next)
    best))

(defn get-best-match-in-tree-seq-for-line-range
  [ts file start-line line]
  (->> ts
       (map get-pos)
       (filter (fn [v]
                 (when-let [f (:file v)]
                   (file-paths-match? file f))))
       (reduce (partial compare-positions-against-line-range
                        start-line
                        line)
               init-best)))

(defn get-ids-from-file-line-range
  [tree file start-line line]
  (util/$- ->> tree
           (tree-seq map? :children)
           (get-best-match-in-tree-seq-for-line-range $
                                                      file
                                                      start-line
                                                      line)
           :ids))
