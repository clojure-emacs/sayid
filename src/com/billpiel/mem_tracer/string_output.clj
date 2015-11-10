(ns com.billpiel.mem-tracer.string-output
  (:require [puget.printer :as puget]))

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


(defn color-code
  [& {:keys [fg bg bold]}]
  (str "\033["
       (->> [(when (#{true 1} bold)
               1)
             (when fg
               (+ 30 (mod fg 10)))
             (when bg
               (+ 40 (mod bg 10)))]
            (remove nil?)
            (clojure.string/join ";"))
       "m"))

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

(def reset-color-code (color-code))

(defn header-indent
  [depth]
  (str depth (clojure.string/join "" (repeat depth "|"))))

(defn indent
  [depth & {:keys [start end]
            :or   {start " " end " "}}]
  (format "%s%s%s"
          start
          (->> depth
               inc
               (range 1)
               (map #(str (color-code :fg* %)
                          "|"))
               (clojure.string/join ""))
          end))

(defn indent-line-breaks
  [s depth & rest]
  (clojure.string/join ""
                       (mapcat (fn [line] [(apply indent depth rest)
                                           line
                                           reset-color-code
                                           "\n"])
                               (clojure.string/split-lines s))))

(defn header->string
  [entry]
  (let [depth (:depth entry)]
    (clojure.string/join "" [(color-code :fg* depth)
                             (indent depth :end "" )
                             (color-code :fg 10 :bg* depth :bold true)
                             "-"
                             (color-code :bg 0)
                             (fg-color-code depth)
                             (:name entry)
                             reset-color-code])))

(defn args-str
  [entry]
  (when-let [args (:args entry)]
    (indent-line-breaks (clojure.string/join "\n"
                                             (map pprint-str
                                                  args))
                        (:depth entry)
                        :end "  ")))

(defn return-str
  [entry]
  (when-let [return (:return entry)]
    (str (indent (:depth entry))
         "return => \n"
         (indent-line-breaks (str (pprint-str return)
                                  "\n")
                             (:depth entry)
                             :end "  "))))

(defn throw-str
  [entry]
  (when-let [thrown (:throw entry)]
    (str (indent (:depth entry))
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
          (:depth entry))
         "\n")))

(defn entry->string
  [entry]
  (->> [(header->string entry)
        "\n"
        (args-str entry)
        (when (some-> entry :children not-empty) (return-str entry))
        (clojure.string/join "" (mapcat entry->string (:children entry)))
        (return-str entry)
        (throw-str entry)]
       (remove nil?)
       (clojure.string/join "")))

(defn print-entry
  [entry]
  (-> entry
      entry->string
      print))
