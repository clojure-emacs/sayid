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

(defn get-temp-root-tree
  []
  (-> trace/*trace-log-parent*
      (select-keys [:depth :path])
      (assoc :children (atom [])
             :inner-path nil)
      atom))

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

(declare produce-recent-tree-atom!)

(defn mk-recent-tree-at-inner-path
  [path-chain recent-trees]
  (if (empty? path-chain)
    (let [new-tree (get-temp-root-tree)]
      (push-to-recent-trees!  new-tree
                              recent-trees)
      new-tree)
    (let [parent (produce-recent-tree-atom! (drop-last path-chain)
                                            recent-trees)
          new-tree (-> (trace/mk-tree :parent @parent)
                       (assoc :inner-path (last path-chain)
                              :parent-path (:path @parent))
                       atom)]
      (push-to-recent-trees! new-tree
                             recent-trees)
      (conj-to-parent! new-tree
                       parent)
      new-tree)))

(defn produce-recent-tree-atom!
  [inner-path-chain recent-trees & {:keys [skip-closed-check]}]
  (if-let [tree-atom (get-recent-tree-at-inner-path (last inner-path-chain)
                                                    recent-trees
                                                    :skip-closed-check skip-closed-check)]
    tree-atom
    (mk-recent-tree-at-inner-path inner-path-chain recent-trees)))

(defn tr-fn
  [recent-trees {:keys [inner-path-chain arg-forms] :as template} f]
  (fn [& args]
    (let [this (-> (produce-recent-tree-atom! inner-path-chain
                                              recent-trees)
                   deref
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
  [recent-trees template mcro v]
  (def rt' @recent-trees)

  (let [tree @(produce-recent-tree-atom! (:inner-path-chain template)
                                         recent-trees
                                         :skip-closed-check true)]
    (-> tree
        (assoc :return v
               :inner-tags [:macro mcro])
        (merge template)
        (update-tree! recent-trees))
    v))


(defn tr-let-ret
  [recent-trees template v]
  (-> template
      :inner-path-chain
      (produce-recent-tree-atom! recent-trees)
      deref
      (merge template)
      (update-in [:let-binds] (comp vec reverse))
      (assoc :return v
             :inner-tags [:let])
      (update-tree! recent-trees))
  v)

(defn tr-let-bind
  [{:keys [inner-path-chain]} recent-trees v bnd-frm val-frm]
  (-> (produce-recent-tree-atom! inner-path-chain recent-trees)
      deref
      (update-in [:let-binds] conj [v bnd-frm val-frm])
      (update-tree! recent-trees)))

(defn tr-if-ret
  [{:keys [inner-path-chain]} recent-trees template v]
  (-> (produce-recent-tree-atom! inner-path-chain recent-trees)
      deref
      (merge template)
      (assoc :return v
             :inner-tags [:if])
      (update-tree! recent-trees))
  v)

(defn tr-if-test
  [{:keys [inner-path-chain]} recent-trees v]
  (-> (produce-recent-tree-atom! inner-path-chain recent-trees)
      deref
      (assoc :args [v])
      (update-tree! recent-trees))
  v)

(defn tr-if-branch
  [{:keys [inner-path-chain]} recent-trees test v]
  (-> (produce-recent-tree-atom! inner-path-chain recent-trees)
      deref
      (assoc-in [:args 1] v)
      (update-tree! recent-trees))
  v)

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

(defn xpand-fn-form
  [head form template]
  (cons (list `tr-fn
              '$$
              `'~template
              (first form))
        (rest form)))

(defn xpand-macro-form
  [head form template]
  (list `tr-macro
        '$$
        `'~template
        (keyword head)
        form))

(defn xpand-let-binds
  [template binds]
  (vec (mapcat (fn [[b v]]
                 `(~b ~v
                      ~'_ (tr-let-bind '~template
                                       ~'$$
                                       ~b
                                       '~b
                                       '~v)))
               (partition 2 binds))))

(defn xpand-let-form
  [[_ binds] [_ _ & frms] template]
  `(tr-let-ret ~'$$
               '~template
               (let ~(xpand-let-binds template
                                      binds)
                 ~@frms)))

(defn xpand-if-form
  [[_ test then else] template]
  (let [template' (assoc template
                         :test-form `'~test)]
    `(tr-if-ret ~'$$
                '~template'
                (if (tr-if-test ~template'
                                ~'$$
                                ~test)
                  (tr-if-branch ~template'
                                ~'$$
                                true
                                ~then)
                  ~(when-not (nil? else)
                     `(tr-if-branch ~template'
                                    ~'$$
                                    false
                                    ~else))))))

(defn get-form-meta-somehow
  [form]
  (or (meta form)
      (-> form first meta)))

(defn xpand-fn
  [head form src-map fn-meta path path-chain]
  (let [path-chain' (conj path-chain path)]
    (xpand-fn-form head
                   (xpand-all form
                              src-map
                              fn-meta
                              path
                              path-chain')
                   (mk-tree-template src-map
                                     (get-form-meta-somehow form)
                                     fn-meta
                                     path-chain'))))

(defn xpand-macro
  [head form src-map fn-meta path path-chain]
  (let [xform (with-meta (macroexpand form) (meta form))] ;; TODO be sure this is doing something
    (let [path' (conj path :macro)
          path-chain' (conj path-chain path)]
      (xpand-macro-form head
                        (xpand-form xform
                                    src-map
                                    fn-meta
                                    path'
                                    path-chain')
                        (mk-tree-template src-map
                                          (get-form-meta-somehow form)
                                          fn-meta
                                          path-chain'
                                          :macro? true)))))

(defn xpand-if
  [form src-map fn-meta path path-chain]
  (xpand-if-form (xpand-all form
                            src-map
                            fn-meta
                            path
                            path-chain)
                 (mk-tree-template src-map
                                   (get-form-meta-somehow form)
                                   fn-meta
                                   path-chain)))

(defn xpand-let
  [form src-map fn-meta path path-chain]
  (let [path-chain' (conj path-chain path)]
    (xpand-let-form form
                    (xpand-all form
                               src-map
                               fn-meta
                               path
                               path-chain')
                    (mk-tree-template src-map
                                      (get-form-meta-somehow form)
                                      fn-meta
                                      path-chain'))))

(defn dot-sym?
  [sym]
  (-> sym
       str
       (.startsWith ".")))

(defn xpand-form
  [form src-map fn-meta & [path path-chain]]
  (let [path' (or path [])
        path-chain' (or path-chain [])
        args [form src-map fn-meta path' path-chain']]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          (= 'let head) (apply xpand-let args)

          (util/macro? head) (apply xpand-macro head args)

          (= 'if head) (apply xpand-if args)

          (or (special-symbol? head)
              (dot-sym? head)) ;; TODO better way to detect these?
          (apply xpand-all args)

          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else form)))

;;TODO try-catch
(defn xpand
  [form parent-fn-meta]
  (let [expr-map (mk-expr-mapping form)
        xform (xpand-form form expr-map parent-fn-meta)]
    `(let [~'$$ (atom {})
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
  [tree-atom]
  (let [children (some-> (@tree-atom nil)
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
    (try (util/eval-in-ns (-> ns' str symbol)
                          traced-form)
         (catch Exception e
           (clojure.pprint/pprint traced-form)
           (throw e)))))

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


#_ (defn f1 [a] (inc 2) (empty? [0 [1 2 (-> a inc dec)]])) ;; TODO


(defn f1 [a] (empty? [0 [1 2 (-> a inc dec)]]))

#_ (deep-tracer {:qual-sym 'com.billpiel.sayid.deep-trace2/f1
                 :meta' nil
                 :ns' 'com.billpiel.sayid.deep-trace2})

#_ (binding [trace/*trace-log-parent* {:id :root1 :children (atom [])}]
  (let [f (deep-tracer {:qual-sym 'com.billpiel.sayid.deep-trace2/f1
                        :meta' nil
                        :ns' 'com.billpiel.sayid.deep-trace2})]
    (f 2)
    (-/p trace/*trace-log-parent*)))

#_ (deep-tracer {:qual-sym 'xpojure.routes.home/home-page
              :meta' nil
              :ns' 'xpojure.routes.home})
