(ns com.billpiel.mem-tracer.util.destructure)

(declare pb)

(defn p-vec [bvec b val]
  (let [gvec (with-meta (gensym "vec__")
               {::placeholder true})]
    (loop [ret (assoc bvec
                      gvec
                      val)
           n 0
           bs b
           seen-rest? false]
      (if (seq bs)
        (let [firstb (first bs)]
          (cond
            (= firstb '&) (recur (pb ret
                                     (second bs)
                                     (drop n (get ret gvec)))
                                 n
                                 (nnext bs)
                                 true)
            (= firstb :as) (pb ret
                               (second bs)
                               gvec)
            :else (if seen-rest?
                    (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                    (recur (pb ret
                               firstb
                               (nth (get ret gvec)
                                    n
                                    nil))
                           (inc n)
                           (next bs)
                           seen-rest?))))
        ret))))

(defn p-map [bvec b v]
  (let [gmap (gensym "map__")
        gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
        defaults (:or b)]
    (loop [ret (-> bvec
                   (assoc gmap
                          (if (seq? v)
                            (clojure.lang.PersistentHashMap/create (seq v))
                            v))
                   ((fn [ret]
                      (if (:as b)
                        (assoc ret (:as b) v)
                        ret))))
           bes (reduce
                (fn [bes entry]
                  (reduce #(assoc %1 %2 ((val entry) %2))
                          (dissoc bes (key entry))
                          ((key entry) bes)))
                (dissoc b :as :or)
                {:keys #(if (keyword? %) % (keyword (str %))),
                 :strs str, :syms #(symbol %)})]
      (if (seq bes)
        (let [bb (key (first bes))
              bk (val (first bes))
              has-default (contains? defaults bb)]
          (recur (pb ret bb (if has-default
                              (get (get ret gmap)
                                   bk
                                   (defaults bb))
                              (get (get ret gmap)
                                   bk)))
                 (next bes)))
        ret))))

(defn pb [bvec b v]
  (cond
    (symbol? b) (assoc bvec
                       (if (namespace b) (symbol (name b)) b)
                       v)
    (keyword? b) (assoc bvec
                        (symbol (name b))
                        v)
    (vector? b) (p-vec bvec b v)
    (map? b) (p-map bvec b v)
    :else (throw (new Exception (str "Unsupported binding form: " b)))))

(defn destructure-to-map [bindings]
  (let [bents (partition 2 bindings)
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if-let [kwbs (seq (filter #(keyword? (first %)) bents))]
      (throw (new Exception (str "Unsupported binding key: " (ffirst kwbs))))
      (reduce process-entry {} bents))))

(defn arity-match?
  [arglist args]
  (let [[a r] (partition-by (partial = '&)
                            arglist)
        ca (count a)
        cargs (count args)]
    (or (= ca cargs)
        (and r
             (< ca cargs)))))

(defn arg-match
  [arglists args]
  (let [ arglist (->> arglists
                      (filter #(arity-match? % args))
                      first)]
    (destructure-to-map (list arglist args))))


#_ (do

     (destructure-to-map '[a 1 b 2])

     (destructure-to-map '[[a b] [1 2] c 3])

     (-> (destructure-to-map '[[a b] [1 2] c 3])
         keys
         first
         meta)

     (destructure-to-map '[[a & b] [1 2 4] c 3])

     (destructure-to-map '[{a :a} {:a 3}])

     (destructure-to-map '[{:keys [a b] :as ab} {:a 1 :b [3 4 5]}
                    [_ {[d e & f] :aa} _] [1 {:aa [1 2 3 4 5 6]} 3]
                    ])

     (defn xxx ([a b] 1) ([a c & b] 2))
     (partition-by (partial = '&)
                   ['a 'b '& 'c])

     (clojure.core/destructure '[{a :a} {:a 3}]))
