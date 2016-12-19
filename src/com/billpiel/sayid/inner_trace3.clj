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
            {(sym->path
              (if (coll? xl)
                (sym-seq->parent xl)
                xl))
             {:orig (-> xl
                        xl->xform
                        xform->form) ;; original symbol or value
              :x (-> xl
                     xl->xform)}})]
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
  [src-map frm-meta fn-meta path & {:keys [macro?]}]
  (let [sub-src-map (->> path
                         rest
                         (remove #{:macro})
                         src-map)
        form (if macro?
               (:orig sub-src-map)
               (:x sub-src-map))]
    {:inner-body-idx (first path)
     :inner-path path
     :name (if (seq? form)
             (first form)
             form)
     :form form
     :macro? macro?
     :parent-name (symbol (format "%s/%s"
                                  (-> fn-meta :ns str)
                                  (:name fn-meta)))
     :ns (-> fn-meta :ns str symbol)
     :xpanded-frm (:x sub-src-map)
     :src-pos (select-keys frm-meta [:line :column :end-line :end-column :file])}))

(defn dot-sym?
  [sym]
  (-> sym
       str
       (.startsWith ".")))

(declare xpand-form)

(defn merge-xpansion-maps
  [ms]
  {:templates (->> ms
                   (map :templates)
                   (apply merge))
   :path-parents (->> ms
                      (map :path-parents)
                      (apply merge))
   :form (map :form ms)})

(defn layer-xpansion-maps
  [bottom top]
  (assoc top
         :templates (->> [bottom top]
                         (map :templates)
                         (apply merge))
         :path-parents (->> [bottom top]
                            (map :path-parents)
                            (apply merge))))

(defn xpand-all
  [form src-map fn-meta path path-parent]
  (when-not (nil? form)
    (let [xmap (merge-xpansion-maps (doall (map-indexed #(xpand-form %2
                                                                     src-map
                                                                     fn-meta
                                                                     (conj path %)
                                                                     path-parent)
                                                        form)))]
      (update-in xmap
                 [:form]
                 (partial util/back-into
                          form)))))

(defn get-form-meta-somehow
  [form]
  (or (meta form)
      (-> form first meta)))

#_(defn xpand-fn-form
  [head form template]
  (cons (list `tr-fn
              '$$
              `'~template
              (first form))
        (rest form)))

(defn xpand-fn-form
  [head form path-sym]
  `(tr-fn ~path-sym
          ~'$$
          ~(first form)
          ~@(rest form)))

(defn xpand-fn
  [head form src-map fn-meta path path-parent]
  (let [xmap (xpand-all form
                        src-map
                        fn-meta
                        path
                        path-parent)]
    (layer-xpansion-maps xmap
                         {:path-parents {path path-parent}
                          :templates {path (mk-tree-template src-map
                                                             (get-form-meta-somehow form)
                                                             fn-meta
                                                             path)}
                          :form (xpand-fn-form head
                                               (:form xmap)
                                               (path->sym path))})))

#_(defn xpand-form
  [form src-map fn-meta & [path path-parent]]
  (let [path' (or path [])
        path-parent' (or path-parent [])
        args [form src-map fn-meta path' path-parent']]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          false "TODO"
          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else form)))

(defn xpand-form
  [form src-map fn-meta path path-parent]
  (let [args [form src-map fn-meta path path-parent]]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          false "TODO"
          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else {:form form})))

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
  [parent-fn-meta idx fn-body]
  (let [[args & tail] fn-body]
    (assoc (xpand (with-meta (vec tail)
                   {:outer true})
                 idx
                 parent-fn-meta)
           :args
           args)))

#_(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map #(xpand-bod % parent-fn-meta)))]
    (cons (first form)
          bods)))

(defn quote* [x] `'~x)

(defn prep-traced-bods
  [traced-bods]
  {:templates (->> traced-bods
                   (map :templates)
                   (apply merge)
                   (mapcat (fn [[k v]]
                             [(path->sym k) (-> v
                                                (update-in [:ns] quote*)
                                                (update-in [:name] quote*)
                                                (update-in [:parent-name] quote*)
                                                (update-in [:form] quote*)
                                                (update-in [:xpanded-frm] quote*)
                                                (assoc :path-parents '$$paths))])))
   :path-parents (->> traced-bods
                      (map :path-parents)
                      (apply merge))
   :form (map (fn [m]
                `(~(:args m)
                  (let [~'$$ (atom [])]
                    ~@(apply list (:form m)))))
              traced-bods)})

(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map-indexed (partial xpand-body
                                        parent-fn-meta))
                  prep-traced-bods)]
    `(let [~'$$paths ~(:paths bods)
           ~@(:templates bods)]
       (fn ~@(:form bods)))))

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

#_(let [$paths {:... :...}
        $0-0-inc (partial tr-fn {:... :...} $paths)]
    (fn [a]
      (let [$$ (atom [])
            $return ($0-0-inc inc $$ a)]
        (record-trace-tree! $$)
        $return)))

#_(let [$paths {:... :...}
        $0-0-inc {:paths $paths
                  :template {:... :...}}]
    (fn [a]
      (let [$$ (atom [])
            $return (tr-fn $0-0-inc $$ inc a)]
        (record-trace-tree! $$)
        $return)))

#_ (inner-tracer {:qual-sym 'com.billpiel.sayid.inner-trace3/f1
                  :meta' {:ns 'com.billpiel.sayid.inner-trace3
                          :name 'com.billpiel.sayid.inner-trace3/f1}
                 :ns' 'com.billpiel.sayid.inner-trace3})
