(ns com.billpiel.sayid.deep-trace
  (require [com.billpiel.sayid.util.other :as util]
           [com.billpiel.sayid.trace :as trace]))


(defn swap-in-path-syms*
  [form func parent & [path]]
  (cond
    (util/special-operator? form) form
    (coll? form)  (util/back-into form
                                  (doall (map-indexed #(swap-in-path-syms* %2
                                                                           func
                                                                           form
                                                                           (conj (or (not-empty path)
                                                                                     [])
                                                                                 %))
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
                       nil))
  ([form]
   (swap-in-path-syms form
                       #(first %&))))


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

(defn mk-inner-tree
  [& {:keys [parent args src-map fn-meta]}]
  (assoc (trace/mk-tree :parent parent)
         :name (:sym src-map)
         :ns (-> fn-meta :ns str symbol)
         :parent-name (:name fn-meta)
         :args (vec args)
         :arg-map (delay (zipmap (-> src-map
                                     :xp
                                     rest)
                                 args))
         :src-map src-map
         :started-at (trace/now)))

(defn trace-inner-form
  [f src-map fn-meta]
  (fn [& args]
    (let [parent trace/*trace-log-parent*
          this (mk-inner-tree :parent parent
                              :fn-meta fn-meta
                              :args args
                              :meta {}
                              :src-map src-map)
          idx  (-> (trace/start-trace (:children parent)
                                      this)
                   count
                   dec)]
      (let [value (binding [trace/*trace-log-parent* this]
                    (try
                      (apply f args)
                      (catch Throwable t
                        (trace/end-trace (:children parent)
                                         idx
                                         {:throw (trace/Throwable->map** t)
                                          :ended-at (trace/now)})
                        (throw t))))]
        (trace/end-trace (:children parent)
                         idx
                         {:return value
                          :ended-at (trace/now)})
        value))))

(defn swap-in-tracer-fn
  [src-maps fn-meta]
  (let [fn-meta' fn-meta]
    (fn swap-in-tracer [s path f parent]
      (if (and (= 0 (last path)) (seq? parent) )
        (let [s-m (util/$- -> src-maps
                           (get s {})
                           (util/apply-to-map-vals (fn [x] `'~x) $))]
          `(trace-inner-form ~f ~s-m '~fn-meta'))
        f))))

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
    [src]
    (let [xls (->> src
                   clojure.walk/macroexpand-all
                   swap-in-path-syms
                   (tree-seq coll? seq)
                   )
          xloc->oloc (util/deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms)
                                  (-> src swap-in-path-syms clojure.walk/macroexpand-all))
          oloc->xloc (clojure.set/map-invert xloc->oloc)
          xl->src  (util/deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms)
                                (clojure.walk/macroexpand-all src))
          ol->olop (-> src
                       swap-in-path-syms
                       get-path->form-maps)
          xl->xlxp (-> src
                       clojure.walk/macroexpand-all
                       swap-in-path-syms
                       get-path->form-maps)
          ol->olxp (-> src
                       swap-in-path-syms
                       clojure.walk/macroexpand-all
                       get-path->form-maps)
          xlxp->xp (util/deep-zipmap (-> src clojure.walk/macroexpand-all swap-in-path-syms)
                                (clojure.walk/macroexpand-all src))
          olop->op (util/deep-zipmap (swap-in-path-syms src) src)
          f (fn [xl]
              {xl {:xl xl
                   :sym (xl->src xl)
                   :xlxp (xl->xlxp xl)
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


(defn deep-tracer
  [{:keys [workspace qual-sym-str meta' ns']} original-fn]
  (let [src (-> qual-sym-str
                symbol
                util/hunt-down-source)
        xsrc (clojure.walk/macroexpand-all src)
        get-fn (fn [[d s f & r]] (if (and (= d 'def)
                                          (symbol? s)
                                          (-> f nil? not)
                                          (nil? r))
                                   f
                                   (throw (Exception. (format "Expected a defn form, but got this (%s %s ..."
                                                              d s)))))
        traced-form  (util/$- ->> src
                              mk-expr-mapping
                              (swap-in-tracer-fn $ meta')
                              (swap-in-path-syms xsrc)
                              get-fn)]
    (util/eval-in-ns (-> ns' str symbol)
                     traced-form)))

(defn ^{::trace/trace-type :deep-fn} composed-tracer-fn
  [m original-fn]
  (util/$- ->>
           original-fn
           (deep-tracer m)
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
