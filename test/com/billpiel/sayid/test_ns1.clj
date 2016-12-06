(ns com.billpiel.sayid.test-ns1
  (:require [com.billpiel.sayid.util.other :refer [$-]]))

(defn func2
  [arg1]
  arg1)

(defn func1
  [arg1]
  (func2 arg1))

(defn func-throws
  [arg1]
  (throw (Exception. (str "Exception from func-throws: " arg1))))

(defn func3-4
  [arg1]
  arg1)

(defn func3-2
  [arg1]
  (* 2 arg1))

(defn func3-2
  [arg1]
  (+ 3 arg1))

(defn func3-3
  [arg1]
  (func3-2 (inc arg1))
  (func3-4 arg1))

(defn func3-1
  [arg1 arg2]
  (func3-2 arg1)
  (func3-3 arg2)
  (+ 2
     (func3-2 arg2)))

(defn print-sleep
  [n]
  (println "Sleeping " n)
  (Thread/sleep n))

(defn func-sleep-30 []
  (Thread/sleep 30))

(defn func-sleep-20 []
  (Thread/sleep 20)
  (func-sleep-30)
  (func-sleep-30))

(defn func-sleep-10 []
  (Thread/sleep 10)
  (func-sleep-20)
  (func-sleep-30)
  (func-sleep-20))

(defn func-identity
  [& args]
  args)

(defn func-complex
  [a b]
  (let [c (* a b)]
    (-> c
        inc
        (+ a)
        vector
        (into [11 22])
        (conj b))))

(defn func-666
  [a b]
  (vector a (+ a (* 2 b))))
