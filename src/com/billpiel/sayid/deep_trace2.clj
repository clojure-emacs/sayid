(ns com.billpiel.sayid.deep-trace2
  (require [com.billpiel.sayid.util.other :as util]
           [com.billpiel.sayid.trace :as trace]))

(defn prs
  [v]
  (subs (with-out-str (clojure.pprint/pprint v)) 0 1000 ))

;; TODO exceptions

(def trace-fn-set #{`tr-if-ret `tr-if-clause `tr-macro})

(defn form->xform-map*
  [form]
  (if (seq? form)
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

(defn update-last
  [vctr f & args]
  (apply update-in
         vctr
         [(-> vctr
              count
              dec)]
         f
         args))

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
  (util/$- -> syms
           first
           (if (coll? $)
             (sym-seq->parent $)
             $)
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
  [form]
  (swap-in-path-syms* form
                      #(first %&)
                      nil
                      []
                      true))

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
             {:tree nil    ;; placeholder for trace tree
              :xl xl              ;; expanded location
              :orig (-> xl
                       xl->xform
                       xform->form)  ;; original symbol or value
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

(defn mk-tree-template
  [src-map frm-meta fn-meta path parent-path & {:keys [macro?]}]

  (let [sub-src-map (-> path
                        path->sym
                        src-map)
        form (if macro?
               (:orig sub-src-map)
               (:x sub-src-map))]
    {:parent-path parent-path
     :name (if (seq? form)
             (first form)
             form)
     :form form
     :macro? macro?
     :inner-path path
     :parent-name (:name fn-meta)
     :ns (-> fn-meta :ns str symbol)
     :xpanded-parent (:xp sub-src-map)
     :xpanded-frm (:x sub-src-map)
     :src-pos (select-keys frm-meta [:line :column :end-line :end-column :file])}))

(defn get-temp-root-tree
  []
  (-> trace/*trace-log-parent*
      (select-keys [:depth :path])
      (assoc :children (atom [])
             :inner-path nil)
      atom))

(declare produce-recent-parent-tree-atom!)

(defn get-recent-tree-at-inner-path
  [path recent-trees & {:keys [skip-closed-check]}]
  (let [entry (@recent-trees path)]
    (if (or skip-closed-check
            (not (some->> entry
                          deref
                          keys
                          (some #{:return :throw}))))
      entry
      nil)))

(defn update-tree!
  [tree recent-trees]
  (reset! (-> tree
              :inner-path
              (@recent-trees))
          tree))

(defn conj-to-parent!
  [tree-atom parent-atom]
  (swap! (:children @parent-atom)
         conj
         tree-atom))

(defn push-to-recent-trees!
  [tree-atom recent-trees]
  (swap! recent-trees
         assoc
         (:inner-path @tree-atom)
         tree-atom))

(defn mk-recent-tree-at-inner-path
  [inner-path recent-trees]
  (if (nil? inner-path)
    (let [new-tree (get-temp-root-tree)]
      (push-to-recent-trees!  new-tree
                              recent-trees)
      new-tree)
    (let [parent (produce-recent-parent-tree-atom! inner-path
                                                   recent-trees)
          new-tree (-> (trace/mk-tree :parent @parent)
                       (assoc :inner-path inner-path
                              :parent-path (:path @parent))
                       atom)]
      (push-to-recent-trees! new-tree
                             recent-trees)
      (conj-to-parent! new-tree
                       parent)
      new-tree)))

(defn produce-recent-tree-atom!
  [inner-path recent-trees & {:keys [skip-closed-check]}]
  (if-let [tree-atom (get-recent-tree-at-inner-path inner-path
                                                         recent-trees
                                                         :skip-closed-check skip-closed-check)]
    tree-atom
    (mk-recent-tree-at-inner-path inner-path
                                  recent-trees)))

(defn produce-recent-parent-tree-atom!
  [inner-path recent-trees]
  (let [parent-inner-path (some-> inner-path
                                  not-empty
                                  drop-last
                                  vec)]
    (if-let [parent (get-recent-tree-at-inner-path parent-inner-path
                                                   recent-trees)]
      parent
      (mk-recent-tree-at-inner-path parent-inner-path recent-trees))))

(defn tr-fn
  [inner-path [recent-trees] template f]
  (fn [& args]
    (let [this (-> @(produce-recent-tree-atom! inner-path
                                               recent-trees)
                   (merge template)
                   (assoc :args (vec args)
                          :arg-map (delay (zipmap (:arg-forms template)
                                                  args))
                          :started-at (trace/now)))
          _ (update-tree! this recent-trees)
          [value throw] (binding [trace/*trace-log-parent* this]
                          (try
                            [(apply f args) nil]
                            (catch Throwable t
                              ;; TODO what's the best we can do here?
                              [nil (trace/Throwable->map** t)])))
          this' (assoc this
                       :return value
                       :throw throw ;;TODO not right
                       :ended-at (trace/now))]
      (update-tree! this'
                    recent-trees)
      value)))

(defn tr-macro
  [path [recent-trees] template mcro v]
  (def rt' @recent-trees)

  (let [tree @(produce-recent-tree-atom! path
                                         recent-trees
                                         :skip-closed-check true)]
    (-> tree
        (assoc :return v
               :inner-tags [:macro mcro])
        (merge template)
        (update-tree! recent-trees))
    v))


(defn tr-let-ret
  [path [recent-trees] template v]
  (-> path
      (produce-recent-tree-atom! recent-trees)
      deref
      (merge template)
      (update-in [:let-binds] (comp vec reverse))
      (assoc :return v
             :inner-tags [:let])
      (update-tree! recent-trees))
  v)

(defn tr-let-bind
  [path [recent-trees] v bnd-frm val-frm]
  (-> path
      (produce-recent-tree-atom! recent-trees)
      deref
      (update-in [:let-binds] conj [v bnd-frm val-frm])
      (#(do #spy/d %))
      (update-tree! recent-trees)))

(defn tr-if-ret
  [path [recent-trees] template v]
  (-> path
      (produce-recent-tree-atom! recent-trees)
      deref
      (merge template)
      (assoc :return v
             :inner-tags [:if])
      (update-tree! recent-trees))
  v)

(defn tr-if-test
  [path [recent-trees] v]
  (-> path
      (produce-recent-parent-tree-atom! recent-trees)
      deref
      (assoc :args [v])
      (update-tree! recent-trees))
  v)

(defn tr-if-branch
  [path [recent-trees] test v]
  (-> path
      (produce-recent-parent-tree-atom! recent-trees)
      deref
      (assoc-in [:args 1] v)
      (update-tree! recent-trees))
  v)

(declare xpand-form)

(defn xpand-all
  [form src-map fn-meta path parent-path]
  (when-not (nil? form)
    (util/back-into form
                    (doall (map-indexed #(xpand-form %2
                                                     src-map
                                                     fn-meta
                                                     (conj path %)
                                                     parent-path)
                                        form)))))

(defn xpand-fn-form
  [head form path template]
  (cons (list `tr-fn
              path
              '$$
              `'~template
              (first form))
        (rest form)))

(defn xpand-macro-form
  [head form path template]
  (list `tr-macro
        path
        '$$
        `'~template
        (keyword head)
        form))

(defn xpand-let-binds
  [path binds]
  (vec (mapcat (fn [[b v]]
                 `(~b ~v
                   ~'_ (tr-let-bind ~path ~'$$ ~b '~b '~v)))
               (partition 2 binds))))

(defn xpand-let-form
  [[_ binds & frms] path template]
  (let [template' template]
    `(tr-let-ret ~path
                 ~'$$
                 '~template'
                 (let ~(xpand-let-binds path
                                        binds)
                   ~@frms))))

(defn xpand-if-form
  [[_ test then else] path template]
  (let [template' (assoc template
                         :test-form `'~test)]
    `(tr-if-ret ~path
                ~'$$
                '~template'
                (if (tr-if-test ~(conj path 1)
                                ~'$$
                                ~test)
                  (tr-if-branch ~(conj path 2)
                                ~'$$
                                true
                                ~then)
                  ~(when-not (nil? else)
                     `(tr-if-branch ~(conj path 3)
                                ~'$$
                                false
                                ~else))))))

(defn xpand-fn
  [head form src-map fn-meta path parent-path]
  (xpand-fn-form head
                 (xpand-all form
                            src-map
                            fn-meta
                            path
                            path)
                 path
                 (mk-tree-template src-map
                                   (or (meta form)
                                       (-> form first meta))
                                   fn-meta
                                   path
                                   parent-path)))

(defn xpand-macro
  [head form src-map fn-meta path parent-path]
  (let [xform (with-meta (macroexpand form) (meta form))] ;; TODO be sure this is doing something
    (xpand-macro-form head
                      (xpand-form xform
                                  src-map
                                  fn-meta
                                  path
                                  path)
                      path
                      (mk-tree-template src-map
                                        (or (meta form)
                                            (-> form first meta))
                                        fn-meta
                                        path
                                        parent-path
                                        :macro? true))))

(defn xpand-if
  [form src-map fn-meta path parent-path]
  (xpand-if-form (xpand-all form
                            src-map
                            fn-meta
                            path
                            path)
                 path
                 (mk-tree-template src-map
                                   (or (meta form)
                                       (-> form first meta))
                                   fn-meta
                                   path
                                   parent-path)))

(defn xpand-let
  [form src-map fn-meta path parent-path]
  (xpand-let-form (xpand-all form
                             src-map
                             fn-meta
                             path
                             path)
                  path
                  (mk-tree-template src-map
                                    (or (meta form)
                                        (-> form first meta))
                                    fn-meta
                                    path
                                    parent-path)))

(defn xpand-form
  [form src-map fn-meta & [path parent-path]]
  (let [path' (or path [])]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          (= 'let head) (xpand-let form src-map fn-meta path' parent-path)

          (util/macro? head)
          (xpand-macro head form src-map fn-meta path' parent-path)

          (= 'if head) (xpand-if form src-map fn-meta path' parent-path)

          (special-symbol? head) (xpand-all form src-map fn-meta path' parent-path)

          :else (xpand-fn head form src-map fn-meta path' parent-path)))

      (coll? form) form ;; TODO traverse
      :else form)))

;;TODO try-catch
(defn xpand
  [form parent-fn-meta]
  (let [expr-map (mk-expr-mapping form)
        xform (xpand-form form expr-map parent-fn-meta)]
    `(let [~'$$ [(atom {})]
           ~'$return ~xform]
       (record-trace-tree! ~'$$)
       ~'$return)))

(defn xpand-bod
  [fn-bod parent-fn-meta]
  (cons (first fn-bod)
        (map #(xpand % parent-fn-meta)
             (rest fn-bod))))

(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map #(xpand-bod % parent-fn-meta)))]
    (cons (first form)
          bods)))

(defn deref-children
  [tree-atom]
  (if (util/atom? tree-atom)
    (do
      (swap! (:children @tree-atom)
             #(mapv deref-children %))
      @tree-atom)
    tree-atom))

(defn record-trace-tree!
  [[tree-atom src-map]]
  (let [children (-> (@tree-atom nil)
                     deref-children
                     :children
                     deref)]
    (doseq [child children]
      (swap! (:children trace/*trace-log-parent*)
             conj
             child))))

(defn get-fn
  [[d s f & r]]
  (if (and (= d 'def)
           (symbol? s)
           (-> f nil? not)
           (nil? r))
    f
    (throw (Exception. (format "Expected a defn form, but got this (%s %s ..."
                               d s)))))

;;TODO trace multi-arity works??
(defn deep-tracer
  [{:keys [workspace qual-sym meta' ns']}] ;; original-fn and workspace not used! IS THAT RIGHT??
  (let [src (-> qual-sym
                symbol
                util/hunt-down-source)
        traced-form (-> src
                        macroexpand
                        get-fn
                        (xpand-fn* meta'))]
    (util/eval-in-ns (-> ns' str symbol)
                     traced-form)))

(defn ^{::trace/trace-type :deep-fn} composed-tracer-fn
  [m _]
  (->> m
       deep-tracer
       (trace/shallow-tracer m)))

(defmethod trace/trace* :deep-fn
  [_ fn-sym workspace]
  (-> fn-sym
      resolve
      (trace/trace-var* (util/assoc-var-meta-to-fn composed-tracer-fn
                                                   ::trace/trace-type)
                        workspace)))

(defmethod trace/untrace* :deep-fn
  [_ fn-sym]
  (-> fn-sym
      resolve
      trace/untrace-var*))


(defn f1 [a] (let [b 4
                   c (inc b)]
               (if (= a b)
                 (-> c inc str keyword)
                 (->> [a b c] (map inc) (map even?)))))

#(do
   (def form1 '(-> 1 inc str))

   (-/p (xpand form1 {:name "name1" :ns "ns1"} ))

   (-/p (eval (xpand form1 {:name "name1" :ns "ns1"} )))

   (def form2 '(str (inc 1) 3))

   (-/p (xpand form2 {:name "name1" :ns "ns1"} ))

   (-/p (eval (xpand form2 {:name "name1" :ns "ns1"} )))

   (def form3 '(if 1 2 3))

   (-/p (xpand form3 {:name "name1" :ns "ns1"} ))

   (-/p (eval (xpand form3 {:name "name1" :ns "ns1"} )))

   (let [parent {:depth 2 :path [:11 :22] :children (atom [])}]
     (binding [trace/*trace-log-parent* parent]
       (eval (xpand form3 {:name "name1" :ns "ns1"} )))
     (-/p parent))

   (let [parent {:depth 2
                 :path [:11 :22]
                 :children (atom [])
                 :args []
                 :name 'dummy
                 :return nil
                 :arg-map {}
                 :id :22}
         traced-fn (deep-tracer {:workspace nil
                                 :qual-sym 'com.billpiel.sayid.deep-trace2/f1
                                 :meta' (meta #'f1)
                                 :ns' 'com.billpiel.sayid.deep-trace2})]
     (binding [trace/*trace-log-parent* parent]
       (-/p (traced-fn 23)))
     (def p' parent)
     (-/p parent))

   ;; ffff

   (comment))
