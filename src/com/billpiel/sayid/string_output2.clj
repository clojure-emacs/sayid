(ns com.billpiel.sayid.string-output2
  (:require [com.billpiel.sayid.workspace :as ws]
            [com.billpiel.sayid.view :as v]
            [com.billpiel.sayid.util.other :as util]
            [tamarin.core :as tam]
            [clojure.zip :as z]
            clojure.string))

(def ^:dynamic *view* (fn [x] {:args true
                               :return true
                               :throw true
                               :selects false}))

(def ^:dynamic *color-palette* [1 3 2 6 5])

(def colors-kw [:black :red :green :yellow :blue :magenta :cyan :white])

(defn apply-color-palette
  [n]
  (when n
    (nth *color-palette*
         (mod n (count *color-palette*)))))

(def line-break-token {:string "\n" :length 1 :line-break true})

(defn render-tkns
  [v]
  (if (-> v meta ::util/recur)
    (tam/render-tokens (apply list 'recur v))
    (tam/render-tokens v)))

(defn tkn
  [s & {:keys [fg fg* bg bg* bold] :as props}]
  (if (= s "\n")
    line-break-token
    (let [s' (cond (string? s) s
                   (sequential? s) (apply str s)
                   :else (pr-str s))]
      (-> props
          (dissoc :fg :fg* :bg :bg* :bold)
          (assoc 
           :string s'
           :length (count s')
           :fg-color (get colors-kw (or fg (apply-color-palette fg*)))
           :bg-color (get colors-kw (or bg (apply-color-palette bg*)))
           :bold bold)))))

(defn mk-lazy-color-fg*-str
  ([s] (mk-lazy-color-fg*-str s 0))
  ([s i] (lazy-cat [(tkn s :fg* i)]
                   (mk-lazy-color-fg*-str s (inc i)))))

(def lazy-color-fg*-pipes (mk-lazy-color-fg*-str "|"))

(defn slinky-pipes
  [len & {:keys [end]}]
  (concat
   (take (- len (count end)) lazy-color-fg*-pipes)
   (if end
     [(tkn end :fg* (dec len))]
     [])
   [(tkn " ")]))

(def slinky-pipes-MZ (memoize slinky-pipes))

(defn indent
  [depth & {:keys [end]}]
  (slinky-pipes-MZ depth
                   :end end))

(defn breaker
  [f coll]
  (let [[head [delim & tail]] (split-with (complement f)
                                          coll)]
    (lazy-cat [head] (if (not-empty tail)
                        (breaker f tail)
                        []))))

(defn get-line-length
  [line]
  (->> line (map :length) (apply +)))

(defn buffer-lines-to-width
  [width column]
  (map (fn [line]
         (let [buf-length (->> line (map :length) (apply +) (- width))]
           (if (> buf-length 0)
             (conj (vec line) {:string (apply str (repeat buf-length " "))
                            :length buf-length})
             line)))
       column))

(defn mk-column-str
  [indent & cols]
  (def i' indent)
  (def c' cols)
  (let [lines (map (partial breaker :line-break) cols)
        max-height (->> lines
                        (map count)
                        (apply max))
        lines' (map #(take max-height (concat % (repeat []))) lines)
        widths (map #(->> %
                          (map get-line-length)
                          (apply max)
                          inc)
                    lines)]
    (apply concat
           (apply interleave
                  indent
                  (conj (mapv buffer-lines-to-width
                              widths
                              lines')
                        (repeat [(tkn "\n")]))))))

(defn multi-line-indent2
  [& {:keys [cols indent-base]}]
  (->> cols
       (apply mk-column-str
              (repeat (indent indent-base)))))

(def multi-line-indent2-MZ  (memoize multi-line-indent2))

(defn get-line-meta
  [v & {:keys [path header?]}]
  (util/$- some-> (or (:src-pos v)
                      (:meta v))
           (select-keys [:line :column :file :end-line :end-column])
           (clojure.set/rename-keys {:line :src-line
                                     :column :src-column
                                     :file :src-file
                                     :end-line :src-end-line
                                     :end-column :src-end-column})
           (assoc :id (:id v)
                  :fn-name (or (:parent-name v)
                               (:name v))
                  :path path
                  :header header?)
           (update-in [:file] #(if (string? %)
                                 (util/get-src-file-path %)
                                 %))
           (assoc $
                  :line-meta? true)))

(defn indent-arg-map
  [tree m]
  (->> m
       (map (fn [[label value]]
              [(get-line-meta tree
                              :path [:arg-map label])
               (multi-line-indent2 :cols [[(tkn [label " =>"])] (render-tkns value)]
                                      :indent-base (:depth tree))]))
       vec))

(defn indent-map
  [tree m]
  (->> m
       (map (fn [[label value]]
              (multi-line-indent2 :cols [[(tkn [label " =>"])] (render-tkns value)]
                                    :indent-base (:depth tree)
                                    :indent-offset  3)))
       vec))

(defn selects-str
  [tree selects]
  (let [sel-fn (fn [sel]
                 (util/get-some (if (vector? sel)
                                  sel
                                  [sel])
                                tree))
        sel-map (util/apply-to-map-vals sel-fn
                                        selects)]
    [(get-line-meta tree)
     (indent-map tree sel-map)]))

(defn throw-str
  [tree]
  (when-let [thrown (:throw tree)]
    [(get-line-meta tree
                      :path [:throw])
     (multi-line-indent2 :cols [[(tkn "THROW" :fg 1 :bg 7)
                                    (tkn " => ")]
                                   (render-tkns thrown)]
                            :indent-base (:depth tree))]))

(defn return-str
  [tree & {pos :pos}]
  (when (contains? tree :return)
    (let [return (:return tree)]
      [(get-line-meta tree
                      :path [:return])
       (multi-line-indent2 :cols [[(tkn [(condp = pos
                                               :before "returns"
                                               :after "returned")
                                            " => "])]
                                     (render-tkns return)]
                              :indent-base (:depth tree))])))

(defn args-map-str
  [tree]
  (when-let [arg-map (:arg-map tree)]
    (indent-arg-map tree arg-map)))

(defn args-str
  [tree]
  (when-let [args (:args tree)]
    (->> args
         (map-indexed (fn [i value]
                        [(get-line-meta tree :path [:args i])
                         (multi-line-indent2 :cols [(render-tkns value)]
                                                :indent-base (:depth tree))]))
         vec)))

(defn let-binds-str
  [tree]
  (->> tree
       :let-binds
       (map-indexed (fn [i [val sym frm]]
                      [(get-line-meta tree :path [:let-binds i 0])
                       (multi-line-indent2 :cols [[(tkn sym)
                                                      (tkn " <= ")]
                                                     (render-tkns val)
                                                     [(tkn " <= ")]
                                                     (render-tkns frm)]
                                              :indent-base (:depth tree))]))
       vec))

(defn args*-str
  [tree]
  (let [test #(-> tree % not-empty)]
    ((cond
       (test :arg-map) args-map-str
       (test :args) args-str
       (test :let-binds) let-binds-str
       :else (constantly (tkn "")))
     tree)))

(defn name->string
  [tree start?]
  (let [{:keys [depth name form ns parent-name macro? xpanded-frm]} tree]
    (if (nil? depth)
      []
      [(get-line-meta tree :header? true)
       (slinky-pipes-MZ depth :end (when start? "v"))
       (when (:throw tree)
         [(tkn "!" :fg 1 :bg 7) (tkn " ")])
       (if parent-name
         [(tkn (if-not (nil? form)
                 [form]
                 name)
               :fg 0 :bg* (dec depth) :bold false)
          (when macro?
            (tkn [" => " (str xpanded-frm)]
                 :fg* (dec depth) :bg 0 :bold false))
          (tkn ["  " (str parent-name)]
               :fg* (dec depth) :bg 0 :bold false)]
         (tkn name :fg* (dec depth) :bg 0 :bold false))
       (tkn "  ")
       (tkn (-> tree :id str)
            :fg 7)])))

(defmacro when-sel
  [kw & body]
  `(when (~kw ~'view)
     [~@body]))

(defn tree->string*
  [tree]
  (if (nil? tree)
    []
    (let [view (*view* tree)
          trace-root? (-> tree meta :trace-root)
          visible? (and (not trace-root?)
                        view)
          has-children (some-> tree
                               :children
                               not-empty)]
      [(when visible?
         [(name->string tree true) (tkn "\n")
          (when-let [selects (:selects view)]
            (selects-str tree selects))
          (when-sel :args (args*-str tree))])
       (when has-children
         [(when-sel :return (return-str tree :pos :before))
          (when-sel :throw (throw-str tree))
          (mapv tree->string* (:children tree))
          (when (not trace-root?)
            [(name->string tree false) (tkn "\n")])
          (when-sel :args
            (args*-str tree))])
       (when visible?
         [(when-sel :return (return-str tree :pos :after))
          (when-sel :throw (throw-str tree))
          (get-line-meta tree) ;; clear meta
          (slinky-pipes-MZ (:depth tree)
                           :end "^")])
       (tkn "\n")])))

(defn increment-position
  [line-break? line column pos]
  (if line-break?
    [(inc line) 0 pos]
    [line column pos]))

(defn assoc-tokens-pos
  [tokens]
  (loop [[{:keys [length line-break line-meta?] :as head} & tail] tokens
         line-meta nil
         pos 0
         line 0
         col 0
         agg []]
    (cond (nil? head) agg
          line-meta? (recur tail head pos line col agg)
          :else (let [end-pos (+ pos length)
                      end-col (+ pos col)
                      [line' col' pos'] (increment-position line-break line end-col end-pos)]
                  (recur tail
                         line-meta
                         pos'
                         line'
                         col'
                         (util/$- -> head
                                  (merge line-meta)
                                  (assoc :line line
                                         :start-col col
                                         :end-col end-col
                                         :start pos
                                         :end end-pos)
                                  (conj agg $)))))))

;; 102
(defn remove-nil-vals
  [m]
  (apply merge (for [[k v] m :when (not (nil? v))] {k v})))

(defn tkn->simple-type
  [t]
  (-> t :type first))

(def ^:const  type->color {:int :cyan
                           :float :cyan
                           :string :magenta
                           :keyword :yellow
                           :symbol :cyan
                           :truncator :black})

(def ^:const  type->bg-color {:truncator :white})

(defn apply-type-colors-to-token
  [token]
  (let [st (tkn->simple-type token)]
    (merge token
           (when-let [color (type->color st)]
             {:fg-color color})
           (when-let [color (type->bg-color st)]
             {:bg-color color}))))

(defn mk-text-props
  [{:keys [start end] :as token}]
  (util/$- -> token
           (dissoc :coll?
                   :column
                   :start
                   :end
                   :end-col
                   :end-column
                   :end-line
                   :length
                   :line
                   :line-meta?
                   :line-break
                   :string
                   :zipper)
           remove-nil-vals
           apply-type-colors-to-token
           [start end $]))

(defn split-text-tag-coll
  [tokens]
  [(->> tokens (map :string) (apply str))
   (->> tokens
        assoc-tokens-pos
        (mapv mk-text-props)
        tf1
        )])


#_(defn tf4
  [pos-pairs start end]
  (cond (empty? pos-pairs) [[start end]]
        :else
        (let [[last-start last-end] (last pos-pairs)]
          (if (= last-end start)
            (conj (vec (drop-last  pos-pairs))
                  [last-start end])
            (conj pos-pairs [start end])))))

(defn tf4
  [pos-pairs start end]
  (cond (empty? pos-pairs) (list end start) 
        :else
        (let [[last-end & r] pos-pairs]
          (if (= last-end start)
            (conj r end)
            (into pos-pairs (list start end))))))

(defn tf3
  [start end agg [k v]]
  (update-in agg [k v] tf4 start end))

(defn tf2
  [agg [start end props]]
  (reduce (partial tf3 start end)
          agg
          props))

(defn tf5
  [m]
  (reduce (fn [agg [a b c]]
            (assoc-in agg [a b] c))
          {}
          (for [[k kv] m
                [k2 v] kv]
            [k k2 (partition 2 (reverse v))])))

(defn tf1
  [triples]
  (def trip1 triples)
  (tf5 (reduce tf2 {} triples)))



#_(def xxx (time (tf1 trip1)))

#_(clojure.pprint/pprint xxx)



#_(def xxo xxx)

#_(clojure.pprint/pprint xxo)

(defn tree->text-prop-pair
  [tree]
  (->> tree
       tree->string*
       flatten
       (remove nil?)
       split-text-tag-coll))

(defn audit-ns->summary-view
  [audit-ns]
  (let [[ns-sym audit-fns] audit-ns
        fn-count (count audit-fns)
        traced-count (->> audit-fns
                          (map second)
                          (map :trace-type)
                          (filter #{:fn :inner-fn})
                          count)]
    (tkn (format "  %s / %s  %s\n" traced-count fn-count ns-sym)
         :ns ns-sym)))

(defn audit-fn->view
  [[ fn-sym {:keys [trace-type trace-selection] :as  fn-meta}]]
  (apply tkn (format "  %s %s %s\n"
                     (case trace-selection
                       :fn "O"
                       :inner-fn "I"
                       :ns " "
                       nil "x"
                       :else "?")
                     (case trace-type
                       :fn "E"
                       :inner-fn "E"
                       nil "D"
                       :else "?")
                     fn-sym)
         (apply concat fn-meta)))

(defn audit-fn-group->view
  [[ns-sym audit-fns]]
  (concat [(tkn (format "- in ns %s\n" ns-sym))]
          (map audit-fn->view audit-fns)))

(defn audit->top-view
  [audit]
  (concat [(tkn "Traced namespaces:\n")]
          (map audit-ns->summary-view (:ns audit))
          [(tkn "\n\nTraced functions:\n")]
          (map audit-fn-group->view (:fn audit))))

(defn audit->ns-view
  [audit & [ns-sym]]
  (concat [(tkn (format "Namespace %s\n" ns-sym) :ns ns-sym)]
          (map audit-fn->view
               (-> audit :ns (get ns-sym)))
          [(tkn "\n\nTraced functions:\n")]
          (map audit-fn->view
               (-> audit :fn (get ns-sym)))))




(defn tree->string [tree]
  (->> tree
       tree->text-prop-pair
       first
       (apply str)))

(defn print-tree [tree]
  (->> tree
       tree->text-prop-pair
       first
       (apply str)
       println))

(defn print-trees
  [trees]
  (doseq [t trees]
    (print-tree t)))

(defn ansi-color-code
  ([] (ansi-color-code {}))
  ([{:keys [fg-color bg-color]}]
   (let [fg (.indexOf colors-kw (or fg-color :white))
         bg (.indexOf colors-kw (or bg-color :black))]
     (->> [(if (= fg -1) nil (+ 30 fg))
           (if (= bg -1) nil (+ 40 bg))]
          (remove nil?)
          not-empty
          (clojure.string/join ";")
          (format "\33[%sm")))))

(defn print-tree-ansi [tree]
  (->> tree
       tree->string*
       flatten
       (remove nil?)
       (map apply-type-colors-to-token)
       (mapcat (fn [t][(ansi-color-code t)
                       (:string t)
                       (ansi-color-code)]))
       (apply str)
       print))

(defn print-trees-ansi
  [trees]
  (doseq [t trees]
    (print-tree t)))

(defn value->text-prop-pair
  [a]
  (->> a
       render-tkns
       flatten
       (remove nil?)
       split-text-tag-coll))

(defn adjusted-pos
  [n]
  (let [n' (or (some-> n :bounds first) n)
        {:keys [start start-line]} n']
    (when (and start start-line)
      (-> start
          inc))))

(defn find-up-node
  [z]
  (let [up (some-> z z/up z/node)]
    (if (= (tkn->simple-type up) :map-entry)
      (some-> z z/up z/up z/node)
      up)))

(defn find-out-node
  [z]
  (let [up (some-> z z/up z/node)]
    (if (= (tkn->simple-type up) :map-entry)
      (some-> z z/up z/up z/node)
      up)))

(defn find-in-node
  [z]
  (let [down (some-> z z/down z/node)]
    (if (= (tkn->simple-type down) :map-entry)
      (some-> z z/down z/down z/right z/node)
      down)))

(defn find-prev-node
  [z]
  (let [up (some-> z z/up z/node)]
    (if (= (tkn->simple-type up) :map-entry)
      (some-> z z/up z/left z/down z/right z/node)
      (some-> z z/left z/node))))

(defn find-next-node
  [z]
  (let [up (some-> z z/up z/node)]
    (if (= (tkn->simple-type up) :map-entry)
      (some-> z z/up z/right z/down z/right z/node)
      (some-> z z/right z/node))))


(declare get-path)

(defn update-last
  [coll f]
  (update-in coll
             [(-> coll count dec)]
             f))

(defn get-path-of-vec-child
  [z]
  (if-let [left (z/left z)]
    (update-last (get-path left) inc)
    (conj (-> z z/up get-path) 0)))

(defn get-path
  [z]
  (if-let [up (z/up z)]
    (let [parent (z/node up)]
      (case (-> parent :type first)
        :map (get-path up)
        :record (get-path up)
        :map-entry (conj (get-path up) (some-> up z/down z/node :string))
        :vector (get-path-of-vec-child z)
        :list (get-path-of-vec-child z)
        :listp (get-path-of-vec-child z)
        :seq  (get-path-of-vec-child z)))
    []))

(defn decorate-token
  [t]
  (let [z (:zipper t)
        out (adjusted-pos (find-out-node z))
        in (adjusted-pos (find-in-node z))
        prev (adjusted-pos (find-prev-node z))
        next (adjusted-pos (find-next-node z))]
    (if (or out in prev next)
      (-> t
          (assoc :neighbors [out in prev next])
          (assoc :path (util/$- some-> t
                                :zipper
                                get-path
                                (clojure.string/join " " $))))
      t)))

(defn split-text-tag-coll*
  [tokens]
  [(->> tokens (map :string) (apply str))
   (->> tokens
        (mapv mk-text-props)
        (remove #(or (nil? (first %)) (nil? (second %)))))])

(defn value->text-prop-pair*
  [a]
  (->> a
       render-tkns
       flatten
       (remove nil?)
       (map decorate-token)
       split-text-tag-coll*))

(defn tokens->text-prop-pair
  [tokens]
  (->> tokens
       flatten
       (remove nil?)
       split-text-tag-coll))
