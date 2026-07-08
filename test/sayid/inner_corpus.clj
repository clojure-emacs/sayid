(ns sayid.inner-corpus
  "Functions exercising a range of Clojure constructs, used as the inner-tracing
  characterization corpus.  Kept in their own namespace so the traced forms are
  small and self-contained.")

(defn arith [a b]
  (let [s (+ a b)]
    (if (> s 10) (* s 2) s)))

(defn threaded [xs]
  (->> xs (map inc) (filter even?) (reduce + 0)))

(defn branchy [n]
  (cond (neg? n) :neg
        (zero? n) :zero
        :else :pos))

(defn whenny [n]
  (when (pos? n)
    (inc n)))

(defn looped [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (recur (inc i) (+ acc i))
      acc)))

(defn letfned [a]
  (letfn [(double-it [n] (* 2 n))]
    (double-it (inc a))))

(defn cased [k]
  (case k
    :a 1
    :b 2
    99))

(defn caught [n]
  (try
    (/ 10 n)
    (catch ArithmeticException _
      :div-by-zero)))

(defn destructured [{:keys [x y]}]
  (+ x y))

(defn multi
  ([a] (multi a 1))
  ([a b] (+ a b)))

(defn nested-calls [a b]
  (arith a (threaded [b b b])))
