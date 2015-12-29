(ns com.billpiel.mem-tracer.util.find-ns
  (:require [swiss.arrows :refer :all]))

(defn unnest-symbol
  [s]
  (cond
    (symbol? s) s

    (when (list? s)
      (-> s first (= 'quote)))
    (recur (second s))

    :else nil))

(defn alias->ns
  [s ref-ns]
  (some-> ref-ns
          ns-aliases
          (get s)
          str
          symbol))

(defn re-find-nses
  [q]
  (when-let [re (some-<>> q
                          name
                          (re-find #"(.*?)\*$")
                          second
                          java.util.regex.Pattern/quote
                          (str <> ".*")
                          re-pattern)]
    (->> (all-ns)
         (filter #(->> %
                       str
                       (re-find re)))
         not-empty)))

(defn search-nses
  [q ref-ns]
  (let [q' (unnest-symbol q)]
    (-> (or (re-find-nses q')
            (alias->ns q' ref-ns)
            q')
        vector
        flatten)))
