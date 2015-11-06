(ns com.billpiel.mem-tracer.test.ns1)


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
