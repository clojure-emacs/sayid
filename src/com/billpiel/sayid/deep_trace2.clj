(ns com.billpiel.sayid.deep-trace2
  (require [com.billpiel.sayid.util.other :as util]
           [com.billpiel.sayid.trace :as trace]))

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
  [src-map fn-meta path parent-path]
  (let [sub-src-map (-> path
                        path->sym
                        src-map)
        form (:orig sub-src-map)]
    {:parent-path parent-path
     :name (if (seq? form)
             (first form)
             form)
     :form form
     :inner-path path
     :parent-name (:name fn-meta)
     :ns (-> fn-meta :ns str symbol)
     :arg-forms (-> sub-src-map
                    :xp
                    rest)}))

(defn get-temp-root-parent
  []
  (-> trace/*trace-log-parent*
      (select-keys [:depth :path])
      (assoc :children (atom [])
             :inner-path nil
             :return ::placeholder)
      atom))

(declare produce-parent-tree-atom)

(defn get-recent-parent-at-inner-path
  [path recent-parents]
  (let [entry (@recent-parents path)]
    (if (not (some->> entry
                      deref
                      keys
                      (some #{:return :throw})))
      entry
      nil)))

(defn assoc-to-recent-parents!
  [tree-atom parent-tree-atom recent-parents]
  (swap! recent-parents
         assoc
         (:inner-path @tree-atom)
         tree-atom)
  (when parent-tree-atom
    (swap! (-> @parent-tree-atom
               :inner-path
               (@recent-parents)
               deref
               :children)
           conj
           tree-atom)))

(defn mk-recent-parent-at-inner-path
  [inner-path recent-parents]
  (if (nil? inner-path)
    (let [new-parent (get-temp-root-parent)]
      (assoc-to-recent-parents! new-parent
                                nil
                                recent-parents)
      new-parent)
    (let [new-grandparent (produce-parent-tree-atom inner-path
                                                    recent-parents)
          new-parent (-> (trace/mk-tree :parent new-grandparent)
                         (assoc :inner-path inner-path)
                         atom)]
      (assoc-to-recent-parents! new-parent
                                new-grandparent
                                recent-parents)
      new-parent)))

(defn produce-parent-tree-atom
  [inner-path recent-parents]
  (let [parent-inner-path (some-> inner-path
                                  not-empty
                                  drop-last
                                  vec)]
    (if-let [parent (get-recent-parent-at-inner-path parent-inner-path
                                                     recent-parents)]
      parent
      (mk-recent-parent-at-inner-path parent-inner-path recent-parents))))

(binding [trace/*trace-log-parent* {:depth 0 :path []}]
  (let [rp (atom {})]
    (-/p [(produce-parent-tree-atom [1 2] rp)
          (produce-parent-tree-atom [1 3] rp)
          rp])))

(defn capture-fn
  [inner-path [recent-parents src-map] template f]
  (fn [& args]
    (let [parent (produce-parent-tree-atom inner-path
                                           recent-parents)
          this (-> @parent
                   (trace/mk-tree :parent)
                   (assoc :args (vec args)
                          :arg-map (delay (zipmap (:arg-forms template)
                                                  args))
                          :started-at (trace/now)))
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
      (assoc-to-recent-parents! (atom this')
                                parent
                                recent-parents)
      value)))

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
  (cons (list `capture-fn
              path
              '$$
              `'~template
              head)
        (rest form)))

(defn xpand-macro-form
  [head form path template]
  (list `tr-macro
        path
        '$$
        `'~template
        (keyword head)
        form))

(defn xpand-if-form
  [[_ test then else] path template]
  (list `tr-if-ret
        path
        '$$
        `'~template
        (concat ['if test
                 (list `tr-if-clause
                       (conj path 2)
                       '$$
                       `'~template
                       true
                       then)]
                (if-not (nil? else)
                  [(list `tr-if-clause
                         (conj path 3)
                         '$$
                         `'~template
                         false
                         else)]
                  []))))


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
                                   fn-meta
                                   path
                                   parent-path)))

(defn xpand-macro
  [head form src-map fn-meta path parent-path]
  (xpand-macro-form head
                    (xpand-form (macroexpand form)
                                src-map
                                fn-meta
                                path
                                path)
                    path
                    (mk-tree-template src-map
                                      fn-meta
                                      path
                                      parent-path)))

(defn xpand-if
  [form src-map fn-meta path parent-path]
  (xpand-if-form (xpand-all form
                            src-map
                            fn-meta
                            path
                            path)
                 path
                 (mk-tree-template src-map
                                   fn-meta
                                   path
                                   parent-path)))

(defn xpand-form
  [form src-map fn-meta & [path parent-path]]
  (let [path' (or path [])]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond (util/macro? head)
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
    `(let [~'$$ [(atom {}) '~expr-map]
           ~'$return ~xform]
       (record-trace-tree ~'$$)
       ~'$return)))

#(do
   (def form1 '(-> 1 inc keyword))

   (-/p (xpand form1 {:name "name1" :ns "ns1"} ))

   (-/p (eval (xpand form1 {:name "name1" :ns "ns1"} )))

   (comment))

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

(defn log->tree1
  [log src-map fn-meta]
  (loop [[[path val & tags] & tail] log
         agg {}]
    (if path
      (let [path' (interleave (repeat :children) path) ;; TODO the map won't preserve order
            sub-map (-> path
                        path->sym
                        src-map)
            agg' (update-in agg path'
                            #(merge %
                                    {:form (:orig sub-map)
                                     :return val
                                     :path path
                                     :tags tags
                                     :src-map sub-map
                                     :fn-meta fn-meta}))]
        (recur tail
               agg'))
      (-> agg
          (get nil)
          (assoc :children
                 (dissoc (:children agg)
                         nil))))))

(defn tree1->trace-tree
  [tree parent src-map]
  (let [{:keys [form return children tags fn-meta path]} tree
        trace-tree (if (-> tags
                           first
                           #{:fn})
                     (second tags)
                     (assoc (trace/mk-tree :parent parent)
                            :name (if (seq? form) (first form) form)
                            :form form
                            :path' path
                            :parent-name (:name fn-meta)
                            :ns (-> fn-meta :ns str symbol)
                            :args []                  ;;TODO
                            :arg-map {}               ;;TODO
                                        ; :src-map src-map
                            :started-at nil
                            :ended-at nil
                            :return return))]
    (assoc trace-tree
           :children
           (mapv #(-> %
                      second
                      (tree1->trace-tree trace-tree
                                         src-map))
                 children))))

#_ (defn record-trace-tree
  [[log src-map]]
  (swap! (:children trace/*trace-log-parent*)
         conj
         (-> @log
             (log->tree1 src-map fn-meta)
             (tree1->trace-tree trace/*trace-log-parent*
                                src-map))))

(defn record-trace-tree
  [[log src-map]]
(-/p log)
  #_ (loop [[head & body] log
         tree-map {}
         mailbox {}]
    (if head
      (let [path (:path head)
            tree-tmplt (tree-tmplts path)
            inbox (mailbox path)
            tree (mk-tree )


            (assoc tree-map
                   path
                   ()
                   )])
      tree-map)))

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
        xsrc (clojure.walk/macroexpand-all src)
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
