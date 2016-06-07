(ns com.billpiel.sayid.string-output
  (:require [puget.printer :as puget]
            [com.billpiel.sayid.workspace :as ws]
            [com.billpiel.sayid.util.other :as util]
            clojure.string))

(def ^:dynamic *sayid-print-length* 15)
(def ^:dynamic *max-chars* 10000)
(def ^:dynamic *max-arg-lines* 15)
(def ^:dynamic *truncate-lines-count* nil)
(def ^:dynamic *selector* {:args true
                           :return true
                           :children true
                           :throw true
                           :selects false})

(def pprint-str #(puget.printer/cprint-str %
                                           {:seq-limit *sayid-print-length*
                                            :color-scheme {
                                        ; syntax elements
                                                           :delimiter [:red]
                                                           :tag       [:red]
                                        ; primitive values
                                                           :nil       [:white]
                                                           :boolean   [:green]
                                                           :number    [:cyan]
                                                           :string    [:magenta]
                                                           :character [:magenta]
                                                           :keyword   [:yellow]
                                                           :symbol    nil

                                        ; special types
                                                           :function-symbol [:cyan]
                                                           :class-delimiter [:cyan]
                                                           :class-name      [:cyan]}}))

(def ^:dynamic *color-palette* [1 3 2 6 5])

(defn apply-color-palette
  [n]
  (nth *color-palette*
       (mod n (count *color-palette*))))

(defn color-code
  [& {:keys [fg bg fg* bg* bold]}]
  (let [ansi #(conj % (+ %2 (mod %3 10)))]
    (->> (cond-> []
           (#{true 1} bold) (conj 1)
           fg (ansi 30 fg)
           bg (ansi 40 bg)
           fg* (ansi 30 (apply-color-palette fg*))
           bg* (ansi 40 (apply-color-palette bg*)))
         (clojure.string/join ";")
         (format "\33[%sm"))))

(def color-code-MZ  (memoize color-code))

(defn mk-lazy-color-fg*
  [& [i]]
  (lazy-cat [(color-code-MZ :fg* i)]
            (mk-lazy-color-fg* (inc i))))

(def lazy-color-fg* (mk-lazy-color-fg* 0))

(defn mk-lazy-color-fg*-str
  [s]
  (interleave lazy-color-fg*
              (repeat s)))

(def lazy-color-fg*-pipes (mk-lazy-color-fg*-str "|"))

(defn slinky-pipes
  [len & {:keys [start end]}]
  (apply str
         (concat
          (take (- (* 2 len) (count end)) lazy-color-fg*-pipes)
          (if end
            [(color-code :fg* (dec len))
             end]
            [])
          [" "])))

(def slinky-pipes-MZ (memoize slinky-pipes))

(def reset-color-code (color-code-MZ))

(defn indent
  [depth & {:keys [end]}]
  (slinky-pipes-MZ depth
                :end end))

(def indent-MZ indent #_ (memoize indent))

(def truncate-msg (str reset-color-code
                       "...<truncated>..."))

(defn truncate-lines
  [strs]
  (cond
    (nil? *truncate-lines-count*) strs
    (> *truncate-lines-count* (count strs)) strs
    :else (util/$- ->
                   strs
                   (take *truncate-lines-count* $)
                   vec
                   (conj truncate-msg))))

(defn indent-line-breaks
  [s depth & rest]
  [""
   (->> s
        clojure.string/split-lines
        truncate-lines
        (mapv (fn [line] [(apply indent-MZ depth rest)
                          line
                          reset-color-code
                          "\n"])))])

(defn get-line-meta
  [v & {:keys [path header?]}]
  (util/$- some-> (or (:src-pos v)
                      (:meta v))
           (select-keys [:line :column :file :end-line :end-column])
           (assoc :id (:id v)
                  :fn-name (or (:parent-name v)
                               (:name v))
                  :path path
                  :header header?)
           (update-in [:file] #(if (string? %)
                                 (util/get-src-file-path %)
                                 %))
           {:src $}))

(defn name->string
  [tree start?]
  (let [{:keys [depth name form ns parent-name macro? xpanded-frm]} tree]
    (if (nil? depth)
      []
      [(get-line-meta tree :header? true)
       (slinky-pipes-MZ depth :end (when start? "v"))
       (if parent-name
         [(color-code-MZ :fg 0 :bg* (dec depth) :bold false)
          (str (if-not (nil? form)
                 form
                 name))
          (when macro?
            [(color-code-MZ :fg* (dec depth) :bg 0 :bold false) " => " (str xpanded-frm)])
          (color-code-MZ :fg* (dec depth) :bg 0 :bold false)
          "  " (str parent-name)]
         [(color-code-MZ :fg* (dec depth) :bg 0 :bold false)
          (str name)])
       "  "
       (color-code-MZ :fg 7)
       (-> tree :id str)
       reset-color-code])))

(defn multi-line-indent
  [& {:keys [label value indent-base indent-offset]}]
  (let [s (pprint-str value)
        mline (some #{\newline} s)]
    [(indent-MZ indent-base)
     label
     (if mline
       ["\n"
        (indent-line-breaks (str s "\n")
                            (+ 2 indent-base) ;; Why does this need to be 2?
                            :end
                            (apply str
                                   (repeat indent-offset
                                           " ")))]
       [s "\n"])]))


(defn remove-ansi
  [s]
  (let [r (re-pattern (str "\33" "\\[.*?m"))]
    (apply str (clojure.string/split s r))))

(defn count-ansi-chrs
  [s]
  (- (count s) (-> s remove-ansi count)))

(defn buffer-string
  [n s]
  (util/$- ->> s
           (concat $ (repeat " "))
           (take (+ n
                    (count-ansi-chrs s)))
           (apply str)))

(defn string->col-str-seq
  [s]
  (let [lines (clojure.string/split-lines s)
        max (->> lines (map remove-ansi) (map count) (apply max))
        lines' (->> lines (map (partial buffer-string max)))]
    (concat lines' (repeat (buffer-string max "")))))

(defn mk-column-str
  [& cols]
  (let [strs (filter string? cols)
        lines (map clojure.string/split-lines strs)
        max-height (->> lines
                        (map count)
                        (apply max)
                        (min (or *truncate-lines-count*
                                 Integer/MAX_VALUE)))
        col-seqs (map #(if (string? %)
                         (string->col-str-seq %)
                         %)
                      cols)]
    (apply str (take (* (inc (count cols)) max-height)
                     (apply interleave (concat col-seqs [(repeat "\n")]))))))

(defn multi-line-indent2
  [& {:keys [cols indent-base]}]
  (->> cols
       (apply mk-column-str
              (repeat (indent-MZ indent-base)))))

(def multi-line-indent-MZ  (memoize multi-line-indent))
(def multi-line-indent2-MZ  (memoize multi-line-indent2))

(defn indent-arg-map
  [tree m]
  (->> m
       (map (fn [[label value]]
              [(get-line-meta tree
                              :path [:arg-map label])
               (multi-line-indent2-MZ :cols [(str label " => ") (pprint-str value)]
                                     :indent-base (:depth tree))]))
       vec))

(defn indent-map
  [tree m]
  (->> m
       (map (fn [[label value]]
              (multi-line-indent2-MZ :cols [ (str label " => ")
                                             (pprint-str value)]
                                    :indent-base (:depth tree)
                                    :indent-offset  3)))
       vec))

(defn return-str
  [tree & {pos :pos}]
  (binding [*truncate-lines-count* 20]
    (when (contains? tree :return)
      (let [return (:return tree)]
        [(get-line-meta tree
                        :path [:return])
         (multi-line-indent2-MZ :cols [(condp = pos
                                         :before "returns"
                                         :after "returned")
                                       " => "
                                       (pprint-str return)]
                                :indent-base (:depth tree))]))))

(defn args-map-str
  [tree]
  (binding [*truncate-lines-count* *max-arg-lines*]
    (when-let [arg-map (:arg-map tree)]
      (indent-arg-map tree arg-map))))

(defn args-str
  [tree]
  (binding [*truncate-lines-count* *max-arg-lines*]
    (when-let [args (:args tree)]
      (->> args
           (map-indexed (fn [i value]
                          [(get-line-meta tree :path [:args i])
                           (multi-line-indent2-MZ :cols [(pprint-str value)]
                                                 :indent-base (:depth tree))]))
           vec))))

(defn let-binds-str
  [tree]
  (->> tree
       :let-binds
       (map #(multi-line-indent2-MZ :cols [(-> % second str)
                                           (str (color-code)
                                                " <= ")
                                           (-> % first pprint-str)
                                           " <= "
                                           (pprint-str (nth % 2))]
                                    :indent-base (:depth tree)))
       vec))

(defn args*-str
  [tree]
  (let [test #(-> tree % not-empty)]
    ((cond
       (test :arg-map) args-map-str
       (test :args) args-str
       (test :let-binds) let-binds-str
       :else (constantly ""))
     tree)))

(defn throw-str
  [tree]
  (when-let [thrown (:throw tree)]
    [ (indent-MZ (:depth tree))
      (color-code-MZ :fg 7 :bg 1 :bold true)
      "THROW"
      reset-color-code
      " => "
      (pprint-str (:cause thrown))
      "\n"
      (indent-line-breaks
       (->> thrown
            :via
            (mapv (fn [v]
                    (let [at (:at v)
                          [c f l] ((juxt :class-name
                                         :file-name
                                         :line-number)
                                   at)]
                      (format "%s %s:%s" c f l))))
            pprint-str)
       (:depth tree))
      "\n"]))

(defn selects-str
  [tree]
  (let [sel-fn (fn [sel]
                 (util/get-some (if (vector? sel)
                                  sel
                                  [sel])
                                tree))
        sel-map (util/apply-to-map-vals sel-fn
                                        (:selects *selector*))]
    (indent-map tree sel-map)))

(defmacro when-sel
  [kw & body]
  `(when (~kw *selector*)
     [~@body]))

(defn tree->string*
  [tree]
  (if (nil? tree)
    []
    (let [has-children (some-> tree
                               :children
                               not-empty)]
      [(name->string tree true) "\n"
       (when-sel :selects  (selects-str tree))
       (when-sel :args (args*-str tree))
       (when-sel :children
                 (when has-children
                   [(when-sel :return (return-str tree :pos :before))
                    (when-sel :throw (throw-str tree))
                    (mapv tree->string* (:children tree))
                    (name->string tree false) "\n"
                    (when-sel :args
                              (args*-str tree))]))
       (when-sel :return (return-str tree :pos :after))
       (when-sel :throw (throw-str tree))
       (when (and (-> tree :depth nil? not)
                  (-> tree
                      meta
                      ::ws/workspace
                      not))
         [(get-line-meta tree) ;; clear meta
          (slinky-pipes-MZ (:depth tree)
                           :end "^")])
       reset-color-code
       "\n"])))

(defn tree->string
  [tree]
  (->> tree
       tree->string*
       flatten
       (remove nil?)
       (remove map?)
       (clojure.string/join "")))



(defn audit-ns->summary-view
  [audit-ns]
  (let [[ns-sym audit-fns] audit-ns
        fn-count (count audit-fns)
        traced-count (->> audit-fns
                          (map second)
                          (map :trace-type)
                          (filter #{:fn :inner-fn})
                          count)]
    [{:src {:ns ns-sym}} (format "  %s / %s  %s\n" traced-count fn-count ns-sym)]))

(defn audit-fn->view
  [[ fn-sym {:keys [trace-type trace-selection] :as  fn-meta}]]
  [{:src fn-meta} (format "  %s %s %s\n"
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
                   fn-sym)])

(defn audit-fn-group->view
  [[ns-sym audit-fns]]
  (concat [(format "- in ns %s\n" ns-sym)]
          (mapcat audit-fn->view audit-fns)))

(defn audit->top-view
  [audit & [ns-sym]]
  (concat ["Traced namespaces:\n"]
          (mapcat audit-ns->summary-view (:ns audit))
          ["\n\nTraced functions:\n"]
          (mapcat audit-fn-group->view (:fn audit))))

(defn audit->ns-view
  [audit & [ns-sym]]
  (concat [{:src {:ns ns-sym}} (format "Namespace %s\n" ns-sym)]
          (mapcat audit-fn->view
                  (-> audit :ns (get ns-sym)))
          [{:src {}} "\n\nTraced functions:\n"]
          (mapcat audit-fn->view
                  (-> audit :fn (get ns-sym)))))




(declare strings-agg-head)

(defn map-agg-head
  [agg [next & tail :as items]]
  (cond (empty? items) agg
        (map? next) #(map-agg-head (concat [next ""]
                                           agg)
                                   tail)
        (string? next) #(strings-agg-head (concat [[next]] agg)
                                          tail)
        (nil? next) #(map-agg-head agg
                                   tail)))

(defn strings-agg-head
  [[agg-head & agg-tail :as agg] [next & tail :as items]]
  (cond (empty? items) (concat [(apply str agg-head)]
                                     agg-tail)
        (string? next) #(strings-agg-head (concat [(conj agg-head next)]
                                                  agg-tail)
                                          tail)
        :else #(map-agg-head (concat [next (apply str agg-head)]
                                     agg-tail)
                             tail)))

(defn group-meta-strings
  [v]
  (let [[head :as all] (util/$- ->> v
                                (trampoline map-agg-head [])
                                reverse
                                (drop-while #{""}))]
    all))


(defn map->kvpair
  [m]
  (let [[[k v]] (vec m)]
    [k v]))

(defmulti merge-tags (fn [k _ _] k))

(defmethod merge-tags :display
  [_ new old]
  (merge old new))

(defmethod merge-tags :src
  [_ new old]
  new)

(defn open-tag
  [tag-map pos open-tags]
  (let [[k v] (map->kvpair tag-map)
        prev-v (k open-tags)]
    (merge open-tags
           {k (util/$- -> v
                       (merge-tags k $ prev-v)
                       (assoc :_start pos)
                       (dissoc :_end))})))

(defn close-tag
  [kw open-tags pos]
  (some-> open-tags
          kw
          (assoc-in [:_end]
                    pos)))

(defn close-all-open
  [agg-tags open-tags pos]
  (->> open-tags
       keys
       (map #(close-tag % open-tags pos))
       (filter #(< (:_start %) (:_end %)))
       (concat agg-tags)))

(defn finalize-tags
  [agg-tags open-tags pos]
  (->> (close-all-open agg-tags open-tags pos)
       (remove nil?)
       (mapv #(vector (:_start %)
                      (:_end %)
                      (dissoc % :_start :_end)))))

(defn do-map
  [head open-tags agg-tags pos]
  [(open-tag head pos open-tags)
   (conj agg-tags (close-tag (-> head first first)
                             open-tags
                             pos))])

(defn do-string
  [s agg-txt pos]
  [(conj agg-txt s)
   (+ pos (count s))])


(defn split-text-tag-coll
  [c]
  (loop [[head & tail] c
         open-tags {}
         agg-txt []
         agg-tags []
         pos 1]
    (cond
      (map? head)
      (let [[a b] (do-map head open-tags agg-tags pos)]
        (recur tail a agg-txt b pos))

      (string? head)
      (let [[a b] (do-string head agg-txt pos)]
        (recur tail open-tags a agg-tags b))

      (nil? head)
      [(apply str agg-txt)
       (finalize-tags agg-tags
                      open-tags
                      pos)])))

(defn color-pair->fg-bg-map
  [a b]
  (let [a' (util/->int a)
        b' (util/->int b)
        ->kw (fn [v] (case (-> v (/ 10.0) Math/floor util/->int)
                       3 :fg
                       4 :bg))]
    (if (every? (some-fn nil? zero?) [a' b'])
      {:fg "--" :bg "--"}
      (merge {}
             (when a'
               {(->kw a') a'})
             (when b'
               {(->kw b') b'})))))

(defn ansi->prop-map
  [ansi]
  (let [rm (re-matcher (re-pattern (str "\33" "\\[(\\d+)?(;(\\d+))?m"))
                       ansi)]
    (loop [[match c1 _ c2] (re-find rm)
           agg {}]
      (if-not (nil? match)
        (recur (re-find rm)
               (merge agg
                      (color-pair->fg-bg-map c1
                                             c2)))
        {:text-color agg}))))

(defn str->ansi-pairs
  [s]
  (let [r (re-pattern (str "([^" "\33" "]*)(("  "\33" ".*?m)+)"))]
    (loop [head []
           tail s]
      (let [[match txt ansi _] (re-find r tail)
            tail' (subs tail (count match))
            ansi' (when ansi
                    {:display (ansi->prop-map ansi)})
            head' (concat head (remove empty? [txt ansi']))]
        (cond (empty? tail') head'
              (and (empty? txt)
                   (empty? ansi)) (concat head' [tail'])
              :else  (recur head'
                            tail'))))))

(defn annotated->text-prop-pair
  [a]
  (->> a
       flatten
       (remove nil?)
       group-meta-strings
       (map #(if (string? %)
               (str->ansi-pairs %)
               %))
       flatten
       split-text-tag-coll))

(defn tree->text-prop-pair
  [tree]
  (->> tree
       tree->string*
       flatten
       (remove nil?)
       group-meta-strings
       (map #(if (string? %)
               (str->ansi-pairs %)
               %))
       flatten
       split-text-tag-coll))

(defn tree->meta
  [tree]
  (loop [[head & tail] tree
         pos 1
         agg (sorted-map)]
    (cond (nil? head) agg

          (= head "\n")
          (recur tail (inc pos) agg)

          :else
          (recur tail pos (assoc agg pos head)))))

(defn tree->string+meta
  [tree]
  (let [flat-tree (->> tree
                       tree->string*
                       flatten
                       (remove nil?))
        tree-str (->> flat-tree
                      (remove map?)
                      (clojure.string/join ""))
        tree-meta (->> flat-tree
                       (filter #(or (map? %)
                                    (and (string? %)
                                         (re-find #"\n+" %))))
                       tree->meta)]
    {:string tree-str
     :meta tree-meta}))

(defn print-tree-unlimited
  [tree]
  (-> tree
      tree->string
      print))

(defn print-tree
  [tree]
  (let [s (with-out-str (print-tree-unlimited tree))
        s' (if (< *max-chars*
                  (count s))
             (str (subs s 0 *max-chars*)
                  truncate-msg)
             s)]
    (print s')))

(defn print-trees
  [trees]
  (doseq [t trees]
    (print-tree t)))

(defn print-trees-unlimited
  [trees]
  (doseq [t trees]
    (print-tree-unlimited t)))
