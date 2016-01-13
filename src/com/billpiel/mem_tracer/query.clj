(ns com.billpiel.mem-tracer.query
  (require [clojure.zip :as z]
           [com.billpiel.mem-tracer.util.tree-query :as tq]))

(defn tree->seq
  [tree]
  (lazy-cat [tree] (mapcat tree->seq (:children tree))))

(defn query-cat
  [qry-fn tree]
  (->> tree
       tree->seq
       (keep #(when (qry-fn %) %))))

(defn query-tree
  [qry-fn tree]
  (let [e' (update-in tree [:children]
                      (partial mapcat
                               (partial query-tree
                                        qry-fn)))]
    (if (qry-fn tree)
      [e']
      (:children e'))))

(defn get-some*
  [f v]
  (if (fn? f)
    (f v)
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

(defmulti exec-query
  (fn [query-type zipr pairs] query-type))

(defmethod exec-query nil
  [_ zipr pairs]
  (let [tag-pred {:a (some-mk-query-fn pairs)}]
    (tq/query zipr
              tag-pred
              (tq/has-all-tags-fn :a))))

(defmethod exec-query :s
  [_ zipr [ancest descen]]
  (let [tag-pred {:a (mk-query-fn ancest)
                  :d (mk-query-fn descen)}]
    (tq/query zipr
              tag-pred
              (some-fn (tq/is-between-fn :a :d)
                       (tq/has-any-tags-fn :a :d)))))

(defmethod exec-query :a
  [_ zipr queries]
  (tq/query zipr
            {:a (some-mk-query-fn queries)}
            (some-fn (tq/has-all-tags-fn :a)
                     (tq/has-descen-fn :a))))

(defn trace->zipper
  [trace]
  (z/zipper map?
            #(-> % :children not-empty)
            #(assoc % :children %2)
            trace))

(defn q-default
  [pairs]
  (let [tag-pred (map (fn [p] `(mk-query-fn ~@p))
                      pairs)
        tag-pred' `({:a (some-fn ~@tag-pred)})]
    `(~@tag-pred' (tq/has-all-tags-fn :a))))

(defn q-segment
  [[ancestor descendant]]
  (let [tag-pred `({:a (mk-query-fn ~@ancestor)
                    :d (mk-query-fn ~@descendant)})]
    `(~@tag-pred (tq/is-between-fn :a :d))))

(defn q-all-ancestors-of-any
  [pairs]
  (let [tag-pred (map (fn [p] `(mk-query-fn ~@p))
                      pairs)
        tag-pred' `({:a (some-fn ~@tag-pred)})]
    `(~@tag-pred' (some-fn (tq/has-all-tags-fn :a)
                          (tq/has-descen-fn :a)))))

(defn q
  [zipr & body]
  (let [[arg r] (if (-> body
                           first
                           vector?)
                     [nil body]
                     [(first body) (rest body)])]
    (exec-query arg zipr r)))

#_ (do

     (comment a ancestors
              d descendants
              p parent
              c children)

     (q ws [:name] "bill")

     (tq/query (trace->zipper *trace??*)
               {:a (mk-query-fn [:name] "bill")}
               (has-all-tags-fn :a))

     (q ws [:name] "bill"
        [:name] "bob")

     (tq/query (trace->zipper *trace??*)
               {:a (some-fn (mk-query-fn [:name] "bill")
                            (mk-query-fn [:name] "bob"))}
               (has-all-tags-fn :a))

     (q ws s
        [:name] "bill"
        [:name] "bob")

     (tq/query (trace->zipper *trace??*)
               {:a (mk-query-fn [:name] "bill")
                :b (mk-query-fn [:name] "bill")}
               (is-between-fn :a :b))



     (q ws a+ [:name] "bill")
     (q ws a* [:name] "bill")
     (q ws s
        [:name] "bill"
        [:name] "bob")
     (q ws S
        [:name] "bill"
        [:name] "bob")



     (comment))
