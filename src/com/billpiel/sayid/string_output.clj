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

(defn name->string
  [tree start?]
  (let [{:keys [depth name ns parent-name]} tree]
    (if name
      ["" (slinky-pipes-MZ depth :end (when start? "v"))
       (color-code-MZ :fg* (dec depth) :bg 0 :bold false)
       name
       (when parent-name
         (format "  %s/%s" ns parent-name))
       "  "
       (color-code-MZ :fg 7)
       (:id tree)
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
       (str s "\n"))]))

(def multi-line-indent-MZ  (memoize multi-line-indent))

(defn indent-map
  [tree m]
  (->> m
       (map #(multi-line-indent-MZ :label (str (first %) " => ")
                                   :value  (second %)
                                   :indent-base (:depth tree)
                                   :indent-offset  3))
       vec))

(defn return-str
  [tree & {pos :pos}]
  (binding [*truncate-lines-count* 20]
    (when (contains? tree :return)
      (let [return (:return tree)]
        (multi-line-indent-MZ :label (str (condp = pos
                                            :before "returns"
                                            :after "returned")
                                          " => ")
                              :value  return
                              :indent-base (:depth tree)
                              :indent-offset  3)))))

(defn args-map-str
  [tree]
  (binding [*truncate-lines-count* *max-arg-lines*]
    (when-let [arg-map-ref (:arg-map tree)]
      (let [arg-map arg-map-ref]
        (indent-map tree arg-map)))))

(defn args-str
  [tree]
  (when-let [args (:args tree)]
    (indent-line-breaks (clojure.string/join "\n"
                                             (map pprint-str
                                                  args))
                        (:depth tree)
                        :end "   ")))

(defn args*-str
  [tree]
  (let [test #(-> tree % not-empty)]
    ((cond
       (test :arg-map) args-map-str
       (test :args) args-str
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
       (slinky-pipes-MZ (:depth tree)
                        :end "^"))
     reset-color-code
     "\n"]))

(defn tree->string
  [tree]
  (->> tree
       tree->string*
       flatten
       (remove nil?)
       (clojure.string/join "")))

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
