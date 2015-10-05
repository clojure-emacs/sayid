(ns com.billpiel.mem-tracer.core-test
  (:require [midje.sweet :refer :all]
            [com.billpiel.mem-tracer.core :as mt]))

(fact "fact"
      (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)
      (let [trace-root (mt/trace-ns 'com.billpiel.mem-tracer.test.ns1)]
        (com.billpiel.mem-tracer.test.ns1/func1 :a)

        (mt/deref-children trace-root) => {}

        (mt/untrace-ns 'com.billpiel.mem-tracer.test.ns1)))

(macroexpand-1 '(mt/trace-ns 'com.billpiel.mem-tracer.test.ns1 log))
