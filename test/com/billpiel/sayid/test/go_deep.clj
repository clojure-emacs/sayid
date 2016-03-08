(ns com.billpiel.sayid.test.go-deep)

(defn go-deep [n & r]
  (if (> n 0)
     (->> (range (dec n) n)
         (concat r)
         (map #(go-deep (dec n) %))
         flatten
         (apply +))
    r))
