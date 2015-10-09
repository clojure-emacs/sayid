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

(println (color-code :bold 1 :bg 1) "hi" (color-code))


(defn fg-color-code
  [n]
  (color-code :fg (nth [1 3 2 6 4 5]
                       (mod n 6))))

(defn bg-color-code
  [n]
  (str "\033[4" (mod n 10) "m"))

(println  (apply str (mapcat #(list (background-color-code %) "hi ") (range 0 20) )))

(def reset-color-code (color-code))

(defn header-indent
  [depth]
  (str depth (clojure.string/join "" (repeat depth "|"))))

(defn indent
  [depth]
  (->> depth
       inc
       (range 1)
       (map #(str (color-code %)
                  "|"))
       (clojure.string/join "")
       (format " %s ")))

(defn indent-line-breaks
  [s depth]
  (clojure.string/join ""
                       (mapcat (fn [line] [(indent depth)
                                           line
                                           reset-color-code
                                           "\n"])
                               (clojure.string/split-lines s))))

(defn header->string
  [entry]
  (let [depth (:depth entry)]
    (clojure.string/join "" [(color-code depth)
                             (indent depth)
                             (:name entry)
                             reset-color-code])))

(defn args-str
  [entry]
  (when-let [args (:args entry)]
    (indent-line-breaks (clojure.string/join "\n"
                                             (map pprint-str
                                                  args))
                        (:depth entry))))

(defn return-str
  [entry]
  (when-let [return (:return entry)]
    (str (indent (:depth entry))
         "return => "
         (pprint-str return)
         "\n")))

(defn entry->string
  [entry]
  (->> [(header->string entry)
        "\n"
        (args-str entry)
        (when (some-> entry :children not-empty) (return-str entry))
        (clojure.string/join "" (mapcat entry->string (:children entry)))
        (return-str entry)]
       (remove nil?)
       (clojure.string/join "")))

(def ex-entry {:id "23622",
               :parent-id "root23621",
               :depth 1,
               :name "com.billpiel.mem-tracer.test.ns1/func1",
               :args [:a :b]
               :return :r
               :children [{:id "23622",
                           :parent-id "root23621",
                           :depth 2,
                           :name "com.billpiel.mem-tracer.test.ns1/func2",
                           :args [:c :d]
                           :return [:r1 :r2 :r3]
                           :children []
                           :started-at #inst "2015-10-05T20:24:43.887-00:00"}]
               :started-at #inst "2015-10-05T20:24:43.887-00:00"})

#_ (println  (entry->string ex-entry))
