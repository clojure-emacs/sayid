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

(defn some-mk-query-fn
  [pairs]
  (->> pairs
       (map (partial apply
                     mk-query-fn))
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
  (let [tag-pred {:a (apply mk-query-fn ancest)
                  :d (apply mk-query-fn descen)}]
    (tq/query zipr
              tag-pred
              (some-fn (tq/is-between-fn :a :d)
                       (tq/has-any-tags-fn :a :d)))))

(defmethod exec-query :a
  [_ zipr pairs]
  (tq/query zipr
            {:a (some-mk-query-fn pairs)}
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
  (let [[arg rest] (if (-> body
                           first
                           vector?)
                     [nil body]
                     [(first body) (rest body)])
        pairs (partition 2 rest)]
    (exec-query arg zipr pairs)))

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
