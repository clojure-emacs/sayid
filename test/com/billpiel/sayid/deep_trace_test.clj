(ns com.billpiel.sayid.deep-trace-test
  (:require [com.billpiel.sayid.deep-trace :as dt]
            [com.billpiel.sayid.trace :as mt]
            [com.billpiel.sayid.workspace :as mw]
            [com.billpiel.sayid.util.other :as util]
            [com.billpiel.sayid.test-utils :as t-utils]
            [midje.sweet :refer :all]))

(def src1 '(defn func1
             [a]
             (-> a inc (* 2))))

(def expected1 {['$2_1_0_0] {:ol '[$2_0]
                             :olop '(defn $1 [$2_0] (-> $3_1 $3_2 ($3_3_0 $3_3_1)))
                             :olxp '([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))
                             :op '(defn func1 [a] (-> a inc (* 2)))
                             :sym '[a]
                             :xl '[$2_1_0_0]
                             :xlop '(defn $1 [$2_1_0_0] (-> $2_1_1_1_1 $2_1_1_1_0 ($2_1_1_0 $2_1_1_2)))
                             :xlxp '([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))
                             :xp '([a] (* (inc a) 2))}
                '$1 {:ol '$1
                     :olop '(defn $1 [$2_0] (-> $3_1 $3_2 ($3_3_0 $3_3_1)))
                     :olxp '(def $1 (fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))))
                     :op '(defn func1 [a] (-> a inc (* 2)))
                     :sym 'func1
                     :xl '$1
                     :xlop '(defn $1 [$2_1_0_0] (-> $2_1_1_1_1 $2_1_1_1_0 ($2_1_1_0 $2_1_1_2)))
                     :xlxp '(def $1 (fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))))
                     :xp '(def func1 (fn* ([a] (* (inc a) 2))))}
                '$2_1_0_0 {:ol '$2_0
                           :olop '[$2_0]
                           :olxp '[$2_0]
                           :op '[a]
                           :sym 'a
                           :xl '$2_1_0_0
                           :xlop '[$2_1_0_0]
                           :xlxp '[$2_1_0_0]
                           :xp '[a]}
                '$2_1_1_0 {:ol '$3_3_0
                           :olop '($3_3_0 $3_3_1)
                           :olxp '($3_3_0 ($3_2 $3_1) $3_3_1)
                           :op '(* 2)
                           :sym '*
                           :xl '$2_1_1_0
                           :xlop '($2_1_1_0 $2_1_1_2)
                           :xlxp '($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)
                           :xp '(* (inc a) 2)}
                '$2_1_1_1_0 {:ol '$3_2
                             :olop '(-> $3_1 $3_2 ($3_3_0 $3_3_1))
                             :olxp '($3_2 $3_1)
                             :op '(-> a inc (* 2))
                             :sym 'inc
                             :xl '$2_1_1_1_0
                             :xlop '(-> $2_1_1_1_1 $2_1_1_1_0 ($2_1_1_0 $2_1_1_2))
                             :xlxp '($2_1_1_1_0 $2_1_1_1_1)
                             :xp '(inc a)}
                '$2_1_1_1_1 {:ol '$3_1
                             :olop '(-> $3_1 $3_2 ($3_3_0 $3_3_1))
                             :olxp '($3_2 $3_1)
                             :op '(-> a inc (* 2))
                             :sym 'a
                             :xl '$2_1_1_1_1
                             :xlop '(-> $2_1_1_1_1 $2_1_1_1_0 ($2_1_1_0 $2_1_1_2))
                             :xlxp '($2_1_1_1_0 $2_1_1_1_1)
                             :xp '(inc a)}
                '$2_1_1_2 {:ol '$3_3_1
                           :olop '($3_3_0 $3_3_1)
                           :olxp '($3_3_0 ($3_2 $3_1) $3_3_1)
                           :op '(* 2)
                           :sym '2
                           :xl '$2_1_1_2
                           :xlop '($2_1_1_0 $2_1_1_2)
                           :xlxp '($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)
                           :xp '(* (inc a) 2)}
                'def {:ol 'def
                      :olop nil
                      :olxp '(def $1 (fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))))
                      :op nil
                      :sym 'def
                      :xl 'def
                      :xlop nil
                      :xlxp '(def $1 (fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))))
                      :xp '(def func1 (fn* ([a] (* (inc a) 2))))}
                'fn* {:ol 'fn*
                      :olop nil
                      :olxp '(fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1)))
                      :op nil
                      :sym 'fn*
                      :xl 'fn*
                      :xlop nil
                      :xlxp '(fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)))
                      :xp '(fn* ([a] (* (inc a) 2)))}
                '(def $1 (fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)))) {:ol '(def $1 (fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))))
                                                                                          :olop nil
                                                                                          :olxp nil
                                                                                          :op nil
                                                                                          :sym '(def func1 (fn* ([a] (* (inc a) 2))))
                                                                                          :xl '(def $1 (fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))))
                                                                                          :xlop nil
                                                                                          :xlxp nil
                                                                                          :xp nil}
                '($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2) {:ol '($3_3_0 ($3_2 $3_1) $3_3_1)
                                                              :olop nil
                                                              :olxp '([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))
                                                              :op nil
                                                              :sym '(* (inc a) 2)
                                                              :xl '($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)
                                                              :xlop nil
                                                              :xlxp '([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))
                                                              :xp '([a] (* (inc a) 2))}
                '($2_1_1_1_0 $2_1_1_1_1) {:ol '($3_2 $3_1)
                                          :olop nil
                                          :olxp '($3_3_0 ($3_2 $3_1) $3_3_1)
                                          :op nil
                                          :sym '(inc a)
                                          :xl '($2_1_1_1_0 $2_1_1_1_1)
                                          :xlop nil
                                          :xlxp '($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)
                                          :xp '(* (inc a) 2)}
                '(fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))) {:ol '(fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1)))
                                                                                 :olop nil
                                                                                 :olxp '(def $1 (fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))))
                                                                                 :op nil
                                                                                 :sym '(fn* ([a] (* (inc a) 2)))
                                                                                 :xl '(fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)))
                                                                                 :xlop nil
                                                                                 :xlxp '(def $1 (fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))))
                                                                                 :xp '(def func1 (fn* ([a] (* (inc a) 2))))}
                '([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)) {:ol '([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1))
                                                                           :olop nil
                                                                           :olxp '(fn* ([$2_0] ($3_3_0 ($3_2 $3_1) $3_3_1)))
                                                                           :op nil
                                                                           :sym '([a] (* (inc a) 2))
                                                                           :xl '([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2))
                                                                           :xlop nil
                                                                           :xlxp '(fn* ([$2_1_0_0] ($2_1_1_0 ($2_1_1_1_0 $2_1_1_1_1) $2_1_1_2)))
                                                                           :xp '(fn* ([a] (* (inc a) 2)))}})

(fact "mk-expr-mapping on simple source"
  (dt/mk-expr-mapping src1)
  => expected1)

(fact "deep trace call to simple function"
  (with-redefs [mt/now (t-utils/mock-now-fn)
                gensym (t-utils/mock-gensym-fn)]
    (binding [mt/*trace-log-parent* {:children (atom []) :parent {}}]
      ((dt/deep-tracer { :workspace {}
                        :qual-sym 'com.billpiel.sayid.test.ns1/func1
                        :meta' (meta #'com.billpiel.sayid.test.ns1/func1)
                        :ns' 'com.billpiel.sayid.test.ns1}
                       com.billpiel.sayid.test.ns1/func1)
       4)
      (->> mt/*trace-log-parent*
           :children
           deref
           (mapv mw/deep-deref!))))
  => [{:args [4]
       :arg-map {'arg1 4}
       :children []
       :depth 0
       :started-at 0
       :ended-at 1
       :id :10
       :name 'func2
       :ns 'com.billpiel.sayid.test.ns1
       :parent-name 'func1,
       :path [:10]
       :return 4
       :src-map {:ol '$3_0
                 :olop '($3_0 $3_1)
                 :olxp '($3_0 $3_1)
                 :op '(func2 arg1)
                 :sym 'func2
                 :xl '$2_1_1_0
                 :xlop '($2_1_1_0 $2_1_1_1)
                 :xlxp '($2_1_1_0 $2_1_1_1)
                 :xp '(func2 arg1)}}])
