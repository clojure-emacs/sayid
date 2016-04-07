(ns com.billpiel.sayid.deep-trace2
  (require [com.billpiel.sayid.util.other :as util]
           [com.billpiel.sayid.trace :as trace]))

(def trace-fn-set #{`tr-if-ret `tr-if-clause `tr-macro})

(defn path->sym
  [path]
  (->> path
       (clojure.string/join "_")
       (str "$")
       symbol))

(defn sym->path
  [sym]
  (util/$- -> sym
           name
           (subs 1)
           (clojure.string/split #"_")
           (remove #(= % "") $)
           (mapv #(Integer/parseInt %) $)))

(defn sym-seq->parent
  [syms]
  (-> syms
      first
      sym->path
      drop-last
      path->sym))

(defn swap-in-path-syms*
  [form func parent path skip-macro?]
  (cond
    (and skip-macro?
         (util/macro? form)) form
    (coll? form)  (util/back-into form
                                  (doall (map-indexed #(swap-in-path-syms* %2
                                                                           func
                                                                           form
                                                                           (conj path %)
                                                                           skip-macro?)
                                                      form)))
    :else (func (->> path
                     (clojure.string/join "_")
                     (str "$")
                     symbol)
                path
                form
                parent)))

(defn swap-in-path-syms
  ([form func]
   (swap-in-path-syms* form
                       func
                       nil
                       []
                       false))
  ([form]
   (swap-in-path-syms form
                       #(first %&))))

(defn swap-in-path-syms-skip-macro
  ([form]
   (swap-in-path-syms* form
                       #(first %&)
                       nil
                       []
                       true)))

(defn deep-replace-symbols
  [smap coll]
  (clojure.walk/postwalk #(if (symbol? %)
                            (or (get smap %)
                                %)
                            %)
                         coll))

(defn get-path->form-maps
  [src]
  (let [sx-seq (->> src
                    (tree-seq coll? seq)
                    (filter coll?))
        pair-fn (fn [form]
                  (interleave (seq  form)
                              (repeat form)))]
    (apply hash-map
           (mapcat pair-fn
                   sx-seq))))



;;  xl     (-> src clojure.walk/macroexpand-all swap-in-path-syms)
;;  src   src
;;  oloc (-> src swap-in-path-syms clojure.walk/macroexpand-all)
;;  x-form (clojure.walk/macroexpand-all src)

;;  xloc->oloc (deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms) (-> src swap-in-path-syms clojure.walk/macroexpand-all))
;;  xl->src  (deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms) (clojure.walk/macroexpand-all src))
;;  ol->olop (-> src swap-in-path-syms get-path->form-maps)
;;  xl->xlxp (-> src clojure.walk/macroexpand-all swap-in-path-syms get-path->form-maps)
;;  ol->olxp (-> src swap-in-path-syms clojure.walk/macroexpand-all get-path->form-maps)
;;  xl->xp (deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms) (clojure.walk/macroexpand-all src))
;;  olop->op (deep-zipmap (swap-in-path-syms src) src)

(defn mk-expr-mapping
  [form]
  (let [xls (->> form
                 clojure.walk/macroexpand-all
                 swap-in-path-syms
                 (tree-seq coll? seq))
        xloc->oloc (util/deep-zipmap (-> form clojure.walk/macroexpand-all swap-in-path-syms)
                                     (-> form swap-in-path-syms-skip-macro clojure.walk/macroexpand-all))
        oloc->xloc (clojure.set/map-invert xloc->oloc)
        xl->form  (util/deep-zipmap (-> form clojure.walk/macroexpand-all swap-in-path-syms)
                                    (clojure.walk/macroexpand-all form))
        ol->olop (-> form
                     swap-in-path-syms
                     get-path->form-maps)
        xl->xlxp (-> form
                     clojure.walk/macroexpand-all
                     swap-in-path-syms
                     get-path->form-maps)
        ol->olxp (-> form
                     swap-in-path-syms
                     clojure.walk/macroexpand-all
                     get-path->form-maps)
        xlxp->xp (util/deep-zipmap (-> form clojure.walk/macroexpand-all swap-in-path-syms)
                                   (clojure.walk/macroexpand-all form))
        olop->op (util/deep-zipmap (swap-in-path-syms form) form)
        f (fn [xl]
            {(if (seq? xl)
               (sym-seq->parent xl)
               xl)
             {:xl xl ;; expanded location
                             :sym (xl->form xl) ;; original symbol or value
                             :xlxp (xl->xlxp xl) ;; expanded locations expanded parent
                             :ol (xloc->oloc xl)
                             :olop (-> xl
                                       xloc->oloc
                                       ol->olop)
                             :xp  (-> xl
                                      xl->xlxp
                                      xlxp->xp)
                             :op (-> xl
                                     xloc->oloc
                                     ol->olop
                                     olop->op)
                             :olxp (-> xl
                                       xloc->oloc
                                       ol->olxp)
                             :xlop (-> xl
                                       xloc->oloc
                                       ol->olop
                                       ((partial deep-replace-symbols oloc->xloc)))}})]
    (apply merge (map f xls))))

(defn tr-macro
  [path [log] mcro v]
  (swap! log conj [path v :macro mcro])
  v)

(defn tr-if-ret
  [path [log] v]
  (swap! log conj [path v])
  v)

(defn tr-if-clause
  [path [log] test v]
  (swap! log conj [path v :if test])
  (let [test-path (-> path
                      drop-last
                      vec
                      (conj 1))]
    (swap! log conj [test-path test :if]))
  v)

(declare xpand*)

(defn xpand**
  [form path]
  (when-not (nil? form)
    (util/back-into form
                    (doall (map-indexed #(xpand* %2
                                                (conj path %))
                                        form)))))

(defn xpand-macro
  [head form path]
  (list `tr-macro
        path
        '$$
        (keyword head)
        form))

#_   (concat
      (list `tr-if-ret
            (concat
             [(list 'if test
                    (list `tr-if-clause
                          true
                          (inject0->1 then)))]
             (if else
               [(list `tr-if-clause
                      false
                      (inject0->1 else))]
               []))))

(defn xpand-if
  [[_ test then else] path]
  (list `tr-if-ret
        path
        '$$
        (concat ['if test
                 (list `tr-if-clause
                       (conj path 2)
                       '$$
                       true
                       then)]
                (if-not (nil? else)
                  [(list `tr-if-clause
                         (conj path 3)
                         '$$
                         false
                         else)]
                  []))))

(defn xpand*
  [form & [path]]
  (if (seq? form)
    (let [[head] form
          path' (or path
                    [])]
      (cond (util/macro? head) (xpand-macro head
                                            (xpand* (macroexpand form) path)
                                            path')
            (= 'if head) (xpand-if (xpand** form path')
                                   path')
            :else (xpand** form path')))
    form))

(defn xpand
  [form]
  (let [expr-map (mk-expr-mapping form)]
    `(let [~'$$ [(atom []) '~expr-map]
           ~'$return ~(xpand* form)]
       (record-trace-tree ~'$$)
       ~'$return)))


(def trace (atom nil))

(defn find-orig-form
  [path src-map]
  (util/first-match (complement nil?)
                    [#spy/d (some-> path
                                    path->sym
                                    src-map
                                    :sym)
                     (some-> path
                             (conj 0)
                             path->sym
                             src-map
                             :op)
                     (some-> path
                             (conj 1)
                             path->sym
                             src-map
                             :op)]))

(defn record-trace-tree
  [[log src-map]]
  #spy/d src-map
  (reset! trace
          (mapv (fn [[path val & tail]]
                  (let [sm (-> path
                               path->sym
                               src-map)
                        orig (find-orig-form path src-map)]
                    {:path path ;; make arg map!
                     :original orig
                     :value val
                     :src-map sm}))
                @log)))

#spy/d (-> (xpand '(let [a 1
                         b 2]
                     (if false
                       a
                       b)))
           eval)



(clojure.walk/macroexpand-all '(cond a b (-> c d e) f))


#_ (do

      (-> (xpand '(let [a 1]
                     (-> a inc inc dec)))
           eval)


#spy/d (eval (xpand '(cond a b (-> c d e) f)))


     ;; 0
     '(if (even? a)
        (-> a inc (* 2))
        0)

     ;; 1
     '(tr-if-ret (if (even? a)
                   (tr-if-clause (tr-macro :-> (-> a inc (* 2))) true)
                   (tr-if-clause 0 false)))

     (clojure.walk/macroexpand-all '(tr-if-ret (if (even? a)
                                                 (tr-if-clause (tr-macro :->
                                                                         (-> a inc (* 2)))
                                                               true)
                                                 (tr-if-clause 0 false))))

     ;; 2
     '(tr-if-ret (if (even? a)
                   (tr-if-clause (tr-macro :->
                                           (* (inc a)
                                              2))
                                 true)
                   (tr-if-clause 0 false)))

     ;; 3
     '(tr-if-ret $0 $ (if ((tr-fn $# $ even?) a)
                        (tr-if-clause (tr-macro :->
                                                ((tr-fn $# $ *)
                                                 ((tr-fn $# $ inc) a)
                                                 2))
                                      true)
                        (tr-if-clause 0 false)))

     (comment))
