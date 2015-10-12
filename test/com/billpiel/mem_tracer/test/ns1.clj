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
