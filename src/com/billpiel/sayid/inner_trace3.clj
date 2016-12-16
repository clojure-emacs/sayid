(ns com.billpiel.sayid.inner-trace3
  (:require [com.billpiel.sayid.util.other :as util]
            [com.billpiel.sayid.trace :as trace]
            clojure.pprint
            clojure.set))

(defn form->xform-map*
  [form]
  (if (coll? form)
    (let [x (macroexpand form)
          xx (clojure.walk/macroexpand-all form)] ;; TODO better way?
      (conj (mapcat form->xform-map* x)
            {form xx}))
    [{form form}]))

(defn form->xform-map
  [form]
  (apply merge (form->xform-map* form)))

(defn xform->form-map
  [form]
  (-> form
      form->xform-map
      clojure.set/map-invert))

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
  [form]
  (swap-in-path-syms* form
                      #(first %&)
                      nil
                      []
                      true))

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
  (util/$- some-> syms
           first
           (if (coll? $)
             (sym-seq->parent $)
             $)
           sym->path
           drop-last
           path->sym))

(defn deep-replace-symbols
  [smap coll]
  (clojure.walk/postwalk #(if (symbol? %)
                            (or (get smap %)
                                %)
                            %)
                         coll))

(defn mk-expr-mapping
  [form]
  (let [xls (->> form
                 clojure.walk/macroexpand-all
                 swap-in-path-syms
                 (tree-seq coll? seq))
        xloc->oloc (util/deep-zipmap (-> form clojure.walk/macroexpand-all swap-in-path-syms)
                                     (-> form swap-in-path-syms-skip-macro clojure.walk/macroexpand-all))
        oloc->xloc (clojure.set/map-invert xloc->oloc)
        xl->xform  (util/deep-zipmap (-> form clojure.walk/macroexpand-all swap-in-path-syms)
                                     (clojure.walk/macroexpand-all form))
        xform->form (xform->form-map form)
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
            {(if (coll? xl)
               (sym-seq->parent xl)
               xl)
             {:tree nil ;; placeholder for trace tree
              :xl xl    ;; expanded location
              :orig (-> xl
                        xl->xform
                        xform->form) ;; original symbol or value
              :x (-> xl
                     xl->xform)
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
    (util/$- ->> xls
             (map f)
             (apply merge))))

(defn record-trace-tree!
  [tree-atom]
  :NOT-IMPLEMENTED)

(defn tr-fn
  [template f tree-atom & args]
  :NOT-IMPLEMENTED)

(defn mk-tree-template
  [src-map frm-meta fn-meta path-chain & {:keys [macro?]}]
  (let [sub-src-map (util/$- -> path-chain
                             last
                             (remove #{:macro} $)
                             path->sym
                             src-map)
        form (if macro?
               (:orig sub-src-map)
               (:x sub-src-map))]
    {:inner-path-chain path-chain
     :name (if (seq? form)
             (first form)
             form)
     :form form
     :macro? macro?
     :inner-path (last path-chain)
     :parent-name (symbol (format "%s/%s"
                                  (-> fn-meta :ns str)
                                  (:name fn-meta)))
     :ns (-> fn-meta :ns str symbol)
     :xpanded-parent (:xp sub-src-map)
     :xpanded-frm (:x sub-src-map)
     :src-pos (select-keys frm-meta [:line :column :end-line :end-column :file])}))

(defn dot-sym?
  [sym]
  (-> sym
       str
       (.startsWith ".")))

(declare xpand-form)

(defn xpand-all
  [form src-map fn-meta path path-chain]
  (when-not (nil? form)
    (util/back-into form
                    (doall (map-indexed #(xpand-form %2
                                                     src-map
                                                     fn-meta
                                                     (conj path %)
                                                     path-chain)
                                        form)))))

(defn get-form-meta-somehow
  [form]
  (or (meta form)
      (-> form first meta)))

(defn xpand-fn-form
  [head form template]
  (cons (list `tr-fn
              '$$
              `'~template
              (first form))
        (rest form)))

(defn xpand-fn
  [head form src-map fn-meta path path-chain]
  (let [path-chain' (conj path-chain path)]
    {:template (mk-tree-template src-map
                                 (get-form-meta-somehow form)
                                 fn-meta
                                 path-chain')
     :form (xpand-fn-form head
                          (xpand-all form
                                     src-map
                                     fn-meta
                                     path
                                     path-chain'))}))

#_(defn xpand-form
  [form src-map fn-meta & [path path-chain]]
  (let [path' (or path [])
        path-chain' (or path-chain [])
        args [form src-map fn-meta path' path-chain']]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          false "TODO"
          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else form)))

(defn xpand-form
  [form src-map fn-meta path parent-path]
  (let [args [form src-map fn-meta path' path-chain']]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          false "TODO"
          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else form)))

#_ (defn xpand
  [form parent-fn-meta]
  (let [expr-map (mk-expr-mapping form)
        xform (xpand-form form expr-map parent-fn-meta)]
    `(let [~'$$ (atom {})
           ~'$return ~xform]
       (record-trace-tree! ~'$$)
       ~'$return)))

(defn xpand
  [form body-idx parent-fn-meta]
  (xpand-form form
              (mk-expr-mapping form)
              parent-fn-meta
              [body-idx]
              []))

#_(defn xpand-bod
  [fn-bod parent-fn-meta]
  (cons (first fn-bod)
        (map #(xpand % parent-fn-meta)
             (rest fn-bod))))

(defn xpand-body
  [parent-fn-meta fn-body idx]
  (let [[args & tail] fn-body']
    (cons args  ;; wrong
          (xpand (with-meta (vec fn-body')
                   {:outer true})
                 idx
                 parent-fn-meta))))

#_(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map #(xpand-bod % parent-fn-meta)))]
    (cons (first form)
          bods)))

(defn prep-traced-bods
  []
  {:templates ['$0-0 {}]
   :path {'$0-1 ['$$$ '$$$]}
   :bods '(([] 123))}
  :NOT-IMPLEMENTED)

(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map-indexed (partial xpand-body
                                        parent-fn-meta))
                  prep-traced-bods)]
    `(let [~@(:templates bods)
           ~'$$p ~(:paths bods)]
       (fn ~@(:bods bods)))))

(defn get-fn
  [[d s f & r]]
  (if (and (= d 'def)
           (symbol? s)
           (-> f nil? not)
           (nil? r))
    f
    (throw (Exception. (format "Expected a defn form, but got this (%s %s ..."
                               d s)))))

(defn inner-tracer
  [{:keys [workspace qual-sym meta' ns']}] ;; original-fn and workspace not used! IS THAT RIGHT??
  (let [src (-> qual-sym
                symbol
                util/hunt-down-source)
        traced-form (-> src
                        macroexpand
                        get-fn
                        (xpand-fn* meta'))]
    (clojure.pprint/pprint traced-form)
    (try (util/eval-in-ns (-> ns' str symbol)
                          traced-form)
         (catch Exception e
           (clojure.pprint/pprint traced-form)
           (throw e)))))

(defn f1
  [a]
  (inc a))

#_(let [$0-0-inc (partial tr-fn {:... :...})]
    (fn [a]
      (let [$$ (atom [])
            $return ($0-0-inc inc $$ a)]
        (record-trace-tree! $$)
        $return)))

#_ (inner-tracer {:qual-sym 'com.billpiel.sayid.inner-trace2/f1
                  :meta' {:ns 'com.billpiel.sayid.inner-trace2
                          :name 'com.billpiel.sayid.inner-trace2/f1}
                 :ns' 'com.billpiel.sayid.inner-trace2})






