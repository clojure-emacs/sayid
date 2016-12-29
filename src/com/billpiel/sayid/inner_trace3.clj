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
            (when-let [xl' (if (coll? xl)
                             (sym-seq->parent xl)
                             xl)]
              {(sym->path xl')
               {:orig (-> xl
                          xl->xform
                          xform->form) ;; original symbol or value
                :x (-> xl
                       xl->xform)}}))]
    (util/$- ->> xls
             (keep f)
             (apply merge))))

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
  (let [children (some-> (@tree-atom [])
                         deref-children
                         :children
                         deref)]
    (doseq [child children]
      (swap! (:children trace/*trace-log-parent*)
             conj
             child))))

(defn get-temp-root-tree
  [path]
  (-> trace/*trace-log-parent*
      (select-keys [:depth :path])
      (assoc :children (atom [])
             :inner-path path)
      atom))

(defn push-to-tree-atom!
  [new-tree tree-atom]
  (swap! tree-atom
         assoc
         (:inner-path @new-tree)
         new-tree))

(declare produce-recent-tree-atom!)

(defn update-tree!
  [tree tree-atom]
  (reset! (-> tree
              :inner-path
              (@tree-atom))
          tree))

(defn conj-to-parent!
  [node-atom parent-atom]
  (swap! (:children @parent-atom)
         conj
         node-atom))

(defn find-closest-parent
  [path path-parents]
  (if-let [parent (path-parents path)]
    parent
    (when-not (or (nil? path) (empty? path))
      (recur (drop-last path)
             path-parents))))

(defn mk-recent-tree-at-inner-path
  [path path-parents tree-atom]
  (if (= (count path) 0) ;; TODO is this the right way to detect root?
    (let [new-tree (get-temp-root-tree path)]
      (push-to-tree-atom!  new-tree
                           tree-atom)
      new-tree)
    (let [parent (produce-recent-tree-atom! (find-closest-parent path path-parents)
                                            path-parents
                                            tree-atom)
          new-tree (-> (trace/mk-tree :parent @parent)
                       (assoc :inner-path path
                              :parent-path (:path @parent))
                       atom)]
      (push-to-tree-atom! new-tree
                          tree-atom)
      (conj-to-parent! new-tree
                       parent)
      new-tree)))

(defn get-recent-tree-at-inner-path
  [path tree-atom & {:keys [skip-closed-check]}]
  (let [entry (@tree-atom path)]
    (if (or skip-closed-check
            (not (some->> entry
                          deref
                          keys
                          (some #{:return :throw}))))
      entry
      nil)))

(defn produce-recent-tree-atom!
  [path path-parents tree-atom & {:keys [skip-closed-check]}]
  (if-let [tree-atom (get-recent-tree-at-inner-path path
                                                    tree-atom
                                                    :skip-closed-check skip-closed-check)]
    tree-atom
    (mk-recent-tree-at-inner-path path path-parents tree-atom)))

(defn tr-fn
  [template tree-atom f & args]
  (let [this (-> (produce-recent-tree-atom! (:inner-path template)
                                            (:path-parents template)
                                            tree-atom)
                 deref
                 (merge template)
                 (assoc :args (vec args)
                        :arg-map nil
                        :started-at (trace/now)))
        _ (update-tree! this tree-atom)
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
                  tree-atom)
    value))

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
   :form (map :form ms)
   :recur (->> ms
               (map :recur)
               (apply clojure.set/union))})

(defn layer-xpansion-maps
  [bottom top]
  (assoc top
         :templates (->> [bottom top]
                         (map :templates)
                         (apply merge))
         :path-parents (->> [bottom top]
                            (map :path-parents)
                            (apply merge))
         :recur (->> [bottom top]
                     (map :recur)
                     (apply clojure.set/union))))

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

(def lazy-recur
  (concat '(recur)
          (for [x (range)]
            `(nth ~'$$return ~x))))

(defn recur-arity
  [v]
  (when (-> v meta ::util/recur)
    (count v)))

(defn mk-recur-handler
  [arity-set]
  `(case (recur-arity ~'$$return)
     ~@(apply concat
              (for [a arity-set]
                [a (take (inc a) lazy-recur)]))
     nil ~'$$return))

(defn tr-macro
  [template tree-atom mcro v]
  (let [tree @(produce-recent-tree-atom! (:inner-path template)
                                         (:path-parents template)
                                         tree-atom
                                         :skip-closed-check true)]
    (-> tree
        (assoc :return v
               :inner-tags [:macro mcro])
        (merge template)
        (update-tree! tree-atom))
    v))

(defn xpand-macro-form
  [head form path-sym]
  (list `tr-macro
        path-sym
        '$$
        (keyword head)
        form))

(defn with-meta-safe
  [v m]
  (try
    (with-meta v m)
    (catch Exception e
      v)))

(defn xpand-macro
  [head form src-map fn-meta path path-parent]
  (let [xform (with-meta-safe (macroexpand form) (meta form))] ;; TODO be sure this is doing something
    (let [path' (conj path :macro)
          xmap (xpand-form xform
                           src-map
                           fn-meta
                           path'
                           path)]
      (layer-xpansion-maps xmap
                           {:path-parents {path path-parent}
                            :templates {path (mk-tree-template src-map
                                                               (get-form-meta-somehow form)
                                                               fn-meta
                                                               path
                                                               :macro? true)}
                            :form (xpand-macro-form head
                                                    (:form xmap)
                                                    (path->sym path))}))))

(defn tr-loop-bind
  [template tree-atom v bnd-frm val-frm]
  (-> (produce-recent-tree-atom! (:inner-path template)
                                 (:path-parents template)
                                 tree-atom)
      deref
      (update-in [:let-binds] conj [v bnd-frm val-frm])
      (update-tree! tree-atom))
  v)

(defn xpand-loop-binds
  [xbinds orig-binds path-sym]
  (vec (mapcat (fn [[xb xv] [ob ov]]
                 `(~ob (tr-loop-bind ~path-sym
                                    ~'$$
                                    ~xv
                                    '~ob
                                    '~ov)))
               (partition 2 xbinds)
               (partition 2 orig-binds))))

(defn tr-loop
  [template tree-atom v]
  (-> (produce-recent-tree-atom! (:inner-path template)
                                 (:path-parents template)
                                 tree-atom)
      deref
      (merge template)
      (assoc :return v
             :inner-tags [:loop])
      (update-tree! tree-atom))
  v)

(defn xpand-loop-form
  [[_ binds & body] orig-binds path-sym recur-arities]
  `(tr-loop ~path-sym
            ~'$$
            (loop ~(xpand-loop-binds binds orig-binds path-sym)
              (let [~'$$return ~@body]
                ~(mk-recur-handler recur-arities)))))

(defn xpand-loop
  [[_ binds :as form] src-map fn-meta path path-parent]
  (let [xmap (xpand-all form
                        src-map
                        fn-meta
                        path
                        path)]
    (layer-xpansion-maps xmap
                         {:path-parents {path path-parent}
                          :templates {path (mk-tree-template src-map
                                                             (get-form-meta-somehow form)
                                                             fn-meta
                                                             path)}
                          :form (xpand-loop-form (:form xmap)
                                                 binds
                                                 (path->sym path)
                                                 (:recur xmap))})))

(defn tr-recur
  [template tree-atom & args]
  (let [args' (with-meta (vec args) {::util/recur true})]
    (-> (produce-recent-tree-atom! (:inner-path template)
                                   (:path-parents template)
                                   tree-atom)
        deref
        (merge template)
        (assoc :return args'
               :inner-tags [:recur])
        (update-tree! tree-atom))
    args'))

(defn xpand-recur-form
  [[_ & form] path-sym]
  `(tr-recur ~path-sym
             ~'$$
             ~@form))

(defn xpand-recur
  [form src-map fn-meta path path-parent]
  (let [xmap (xpand-all form
                        src-map
                        fn-meta
                        path
                        path)]
    (layer-xpansion-maps xmap
                         {:path-parents {path path-parent}
                          :templates {path (mk-tree-template src-map
                                                             (get-form-meta-somehow form)
                                                             fn-meta
                                                             path)}
                          :form (xpand-recur-form (:form xmap)
                                                  (path->sym path))
                          :recur #{(-> xmap :form count dec)}})))



(defn tr-if-ret
  [template tree-atom v]
  (-> (produce-recent-tree-atom! (:inner-path template)
                                 (:path-parents template)
                                 tree-atom)
      deref
      (merge template)
      (assoc :return v
             :inner-tags [:if])
      (update-tree! tree-atom))
  v)

(defn tr-if-test
  [template tree-atom v]
  (-> (produce-recent-tree-atom! (:inner-path template)
                                 (:path-parents template)
                                 tree-atom)
      deref
      (assoc :args [v])
      (update-tree! tree-atom))
  v)

(defn tr-if-branch
  [template tree-atom v]
  (-> (produce-recent-tree-atom! (:inner-path template)
                                 (:path-parents template)
                                 tree-atom)
      deref
      (assoc-in [:args 1] v)
      (update-tree! tree-atom))
  v)

(defn xpand-if-form
  [[_ test then else] path-sym]
  `(tr-if-ret ~path-sym
              ~'$$
              (if (tr-if-test ~path-sym
                              ~'$$
                              ~test)
                (tr-if-branch ~path-sym
                              ~'$$
                              ~then)
                ~(when-not (nil? else)
                   `(tr-if-branch ~path-sym
                                  ~'$$
                                  ~else)))))

(defn xpand-if
  [form src-map fn-meta path path-parent]
  (let [xmap (xpand-all form
                        src-map
                        fn-meta
                        path
                        path)]
    (layer-xpansion-maps xmap
                         {:path-parents {path path-parent}
                          :templates {path (mk-tree-template src-map
                                                             (get-form-meta-somehow form)
                                                             fn-meta
                                                             path)}
                          :form (xpand-if-form (:form xmap)
                                               (path->sym path))})))

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
                        path)]
    (layer-xpansion-maps xmap
                         {:path-parents {path path-parent}
                          :templates {path (mk-tree-template src-map
                                                             (get-form-meta-somehow form)
                                                             fn-meta
                                                             path)}
                          :form (xpand-fn-form head
                                               (:form xmap)
                                               (path->sym path))})))

(defn dot-sym?
  [sym]
  (-> sym
      str
      (.startsWith ".")))

(defn xpand-form
  [form src-map fn-meta path path-parent]
  (let [args [form src-map fn-meta path path-parent]]
    (cond
      (seq? form)
      (let [head (first form)]
        (cond
          (= 'fn* head) {:form form}
          (util/macro? head) (apply xpand-macro head args)
          (= 'loop head) (apply xpand-loop args)
          (= 'recur head) (apply xpand-recur args)
          (= 'if head) (apply xpand-if args)
          (or (special-symbol? head)
              (dot-sym? head)) ;; TODO better way to detect these?
          (apply xpand-all args)
          :else (apply xpand-fn head args)))

      (coll? form) (apply xpand-all args)
      :else {:form form})))

(defn xpand
  [form body-idx parent-fn-meta]
  (xpand-form form
              (mk-expr-mapping form)
              parent-fn-meta
              [body-idx]
              []))

(defn xpand-body
  [parent-fn-meta idx fn-body]
  (let [[args & tail] fn-body]
    (assoc (xpand (with-meta (vec tail)
                   {:outer true})
                 idx
                 parent-fn-meta)
           :body-idx idx
           :args args)))

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
                  (let [~'$$ (atom {})
                        ~'$$return (do ~@(apply list (:form m)))]
                    (record-trace-tree! ~'$$)
                    ~(mk-recur-handler (:recur m)))))
              traced-bods)})

(defn xpand-fn*
  [form parent-fn-meta]
  (let [bods (->> form
                  rest
                  (map-indexed (partial xpand-body
                                        parent-fn-meta))
                  prep-traced-bods)]
    `(let [~'$$paths ~(:path-parents bods)
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


(defn ^{::trace/trace-type :inner-fn} composed-tracer-fn
  [m _]
  (->> m
       inner-tracer
       (trace/shallow-tracer m)))

(defmethod trace/trace* :inner-fn
  [_ fn-sym workspace]
  (-> fn-sym
      resolve
      (trace/trace-var* (util/assoc-var-meta-to-fn composed-tracer-fn
                                                   ::trace/trace-type)
                        workspace)))

(defmethod trace/untrace* :inner-fn
  [_ fn-sym]
  (-> fn-sym
      resolve
      trace/untrace-var*))


(defn f1
  [a]
  (for [b [a (inc a)]]
    b))

#_ (inner-tracer {:qual-sym 'com.billpiel.sayid.inner-trace3/f1
                  :meta' {:ns 'com.billpiel.sayid.inner-trace3
                          :name 'com.billpiel.sayid.inner-trace3/f1}
                 :ns' 'com.billpiel.sayid.inner-trace3})

#_ (binding [trace/*trace-log-parent* {:id :root1 :children (atom [])}]
     (let [f1 (inner-tracer {:qual-sym 'com.billpiel.sayid.inner-trace3/f1
                            :meta' {:ns 'com.billpiel.sayid.inner-trace3
                          :name 'com.billpiel.sayid.inner-trace3/f1}
                            :ns' 'com.billpiel.sayid.inner-trace3})]
       (f1 2)
       (clojure.pprint/pprint trace/*trace-log-parent*)))

#_ (binding [trace/*trace-log-parent* @com.billpiel.sayid.core/workspace]
     (let [f1 (inner-tracer {:qual-sym 'com.billpiel.sayid.inner-trace3/f1
                             :meta' {:ns 'com.billpiel.sayid.inner-trace3
                                     :name 'com.billpiel.sayid.inner-trace3/f1}
                             :ns' 'com.billpiel.sayid.inner-trace3})]
       (f1 1)
       (clojure.pprint/pprint trace/*trace-log-parent*)))

