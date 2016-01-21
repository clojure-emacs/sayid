(ns com.billpiel.mem-tracer.string-output
  (:require [puget.printer :as puget]
            clojure.string))

(def pprint-str #(puget.printer/cprint-str %
                                           {:color-scheme { ; syntax elements
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

(defn base-indent-slinky
  [& {:keys [override underride]
      :or {override [] underride []}}]
  #spy/d (concat override
          [:start " "
           :end " o o  "
           :default (fn [i]
                      {:fg* i :text "|"})]
          underride))

(defn arg-indent-slinky
  [& {:keys [start end]
      :or {start " "
           end " "}}]
  (base-indent-slinky :override [:start start
                                 -1 "-"
                                 :end end]))

(defn fn-header-indent-slinky
  [& [start?]]
  (apply base-indent-slinky (when start?
                              [:override
                               [-1 (fn [i]
                                     {:fg* i :text "v"})]])))

(defn fn-footer-indent-slinky
  []
  (base-indent-slinky :override [-1 (fn [i]
                                      {:fg* i :text "^"})]))

(defn apply-color-palette
  [n]
  (nth [1 3 2 6 4 5]
       (mod n 6)))

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

(defn slicky-match
  [i len sl]
  (when-not (-> sl first #{:start :end})
    (let [[fi se] sl
          [lo hi] (cond (sequential? fi) fi
                        (= :default fi) [java.lang.Integer/MIN_VALUE java.lang.Integer/MAX_VALUE]
                        (number? fi) [fi fi])]
      (when (or (<= lo i hi)
                (<= lo (- i len) hi))
        se))))

(defn slinky-first-match
  [sl i len]
  (some (partial slicky-match i len)
        (partition 2 sl)))

(defn slinky-map->str
  [m]
  (str (->> (dissoc m :text)
            (mapcat identity)
            (apply color-code))
       (:text m)))

(defn slinky-part->str
  [p n]
  (cond
    (string? p) p
    (fn? p) (recur (p n) n)
    (map? p) (slinky-map->str p)))

(defn slinky->str
  [sl n]
  (let [[& {:keys [start end]}] sl]
    (->> [[(slinky-part->str start nil)]
          (map #(slinky-part->str
                 (slinky-first-match sl
                                     %
                                     n)
                 %)
               (range 0 n))
          [(slinky-part->str end nil)]]
         (apply concat)
         (apply str))))

(def reset-color-code (color-code))

(defn indent
  [depth & rest]
  (slinky->str (apply base-indent-slinky rest)
               depth))

(defn indent-line-breaks
  [s depth & rest]
  (clojure.string/join ""
                       (mapcat (fn [line] [(apply indent depth rest)
                                           line
                                           reset-color-code
                                           "\n"])
                               (clojure.string/split-lines s))))

(defn name->string
  [tree start?]
  (let [{:keys [depth name]} tree]
    (if name
      (clojure.string/join "" [(slinky->str (fn-header-indent-slinky start?) depth)
                               (color-code :fg* (dec depth) :bg 0 :bold false)
                               (:name tree)
                               "  "
                               (color-code :fg 7)
                               (:id tree)
                               reset-color-code]))))

(defn multi-line-indent
  [& {:keys [label value indent-base indent-offset]}]
  (let [s (pprint-str value)
        mline (some #{\newline} s)]
    (str (indent indent-base)
         label
         (if mline
           (str "\n"
                (indent-line-breaks (str s "\n")
                                    indent-base
                                    :override [:end " x x x  "]))
           (str s))
         "\n")))

(defn return-str
  [tree & {pos :pos}]
  (when-let [return (:return tree)]
    (multi-line-indent :label (str (condp = pos
                                     :before "returns"
                                     :after "returned")
                                   " => ")
                       :value  return
                       :indent-base (:depth tree)
                       :indent-offset  2)))

(defn args-map-str
  [tree]
  (when-let [args (:arg-map tree)]
    (apply str
           (map #(multi-line-indent :label (first %)
                                    :value  (second %)
                                    :indent-base (:depth tree)
                                    :indent-offset  2)))))

#_ (defn args-map-str
     [tree]
     (when-let [args (:arg-map tree)]
       (let [lines (map (fn [[a v]] (str a " => " (pprint-str v)))
                        args)]
         (indent-line-breaks (clojure.string/join "\n"
                                                  lines)
                             (:depth tree)
                             :end "   "))))

(defn args-str
  [tree]
  (when-let [args (:args tree)]
    (indent-line-breaks (clojure.string/join "\n"
                                             (map pprint-str
                                                  args))
                        (:depth tree)
                        :end "   ")))

(defn throw-str
  [tree]
  (when-let [thrown (:throw tree)]
    (str (indent (:depth tree))
         (color-code :fg 7 :bg 1 :bold true)
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
         "\n")))

(defn tree->string
  [tree & {:keys [post-head pre-args post-args pre-ret post-ret pre-ex post-ex children]
            :or {post-head true pre-args true post-args true pre-ret true post-ret true pre-ex true post-ex true children true}}]
  (let [has-children (some-> tree
                             :children
                             not-empty)]
    (->> [[(name->string tree true) "\n"]
          (when pre-args
            (args-str tree))
          (when has-children
            [(when pre-ret
               (return-str tree :pos :before))
             (when pre-ex
               (throw-str tree))
             (mapcat tree->string
                     (:children tree))
             (when post-head
               [(name->string tree false) "\n"])
             (when pre-args
               (args-str tree))])
          (when post-ret
            (return-str tree :pos :after))
          (when post-ex
            (throw-str tree))
          (slinky->str (fn-footer-indent-slinky)
                       (:depth tree))
          reset-color-code
          "\n"]
         flatten
         (remove nil?)
         (clojure.string/join ""))))

(defn print-tree
  [tree]
  (-> tree
      tree->string
      print))

(defn print-trees
  [trees]
  (doall (map print-tree
              trees)))

#_ (print (let [v ["a" "bc" "def" "ghijklmnop" "qr" "stu"]
                col-width (inc (apply max (map count v)))]
            (clojure.string/join "\n"
                                 (map #(apply str (concat (take col-width
                                                                (concat %
                                                                        (repeat \space)))
                                                          [\x]))
                                      v))))
