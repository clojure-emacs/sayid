(ns com.billpiel.mem-tracer.core-test
  (:require [midje.sweet :refer :all]
            [com.billpiel.mem-tracer.core :as mt]
            com.billpiel.mem-tracer.test.ns1))

(fact "fact"
      (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)
      (let [trace-root (mt/trace-ns 'com.billpiel.mem-tracer.test.ns1)]
        (com.billpiel.mem-tracer.test.ns1/func1 :a)

        (mt/deref-children trace-root) => {}

        (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)))

(comment "
TODO
- wrap args that are funcs
 - and deep search values for funcs?
- wrap returns that are funcs
 - and deep search values for funcs?
- string output like tools.trace
 - requires sequential log?
- diff entries
")
