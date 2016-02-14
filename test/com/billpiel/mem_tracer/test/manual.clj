(ns com.billpiel.mem-tracer.test.manual
  (:require [com.billpiel.mem-tracer.core :as mm]
            [com.billpiel.mem-tracer.profiling :as pro]
            [com.billpiel.mem-tracer.query2 :as q2]
            [com.billpiel.mem-tracer.test.go-deep :as gd]))

(defn trace-go-deep
  [n rec-sym]
  (mm/ws-reset!)
  (println (mm/ws-add-trace-ns! com.billpiel.mem-tracer.test.go-deep))
  (println "traced: (go-deep n)")
  (time (gd/go-deep n))
  (mm/ws-remove-all-traces!)
  (println "(mm/rec-load-from-ws! :f)")
  (time (mm/rec-load-from-ws! :f))
  (mm/rec-save-as! rec-sym))

(defn compare-trace-go-deep
  [n rec-sym]
  (mm/ws-remove-all-traces!)
  (mm/ws-reset!)
  (println "(go-deep n)")
  (time (gd/go-deep n))
  (trace-go-deep n rec-sym))

(declare rp-output)

(defn trace-and-prof-printing-go-deep
  [n rec-sym]
  (trace-go-deep n 'dummy)
  (mm/w-rat!)

  (mm/w-atn! 'com.billpiel.mem-tracer.string-output)
  (println (mm/ws-show-traced))
  (println "(with-out-str (mm/r-print)) *****")
  (time (def rp-output (with-out-str (mm/r-print))))
  (println (count rp-output))
  (println "****************************")
  (mm/w-rat!)
  (println "(mm/r-lfw!)")
  (time (mm/r-lfw!))
  (println "(mm/r-sa! 'temp)")
  (time (mm/r-sa! 'temp))
  (println "(mm/r-lf! (pro/add-metrics-to-rec $rec/temp) :f)")
  (time (mm/r-lf! (pro/add-metrics-to-rec @(ns-resolve '$rec 'temp)) :f))
  (println "(time (mm/r-sa! rec-sym))")
  (time (mm/r-sa! rec-sym))
  (println "done"))


(defn trace-and-prof-profiling-go-deep
  [n rec-sym]
  (trace-go-deep n 'dummy)
  (mm/w-rat!)
  (mm/w-atn! 'com.billpiel.mem-tracer.profiling)
  (println (mm/ws-show-traced))

  (println "(mm/r-lfw!)")
  (time (mm/r-lfw!))
  (println "(mm/r-sa! 'ptemp)")
  (time (mm/r-sa! 'ptemp))
  (println "(mm/r-lf! (pro/add-metrics-to-rec $rec/ptemp) :f)")
  (time (def temp-amtr (pro/add-metrics-to-rec @(ns-resolve '$rec 'ptemp))))

  (mm/w-rat!)
  (println "(mm/r-lfw!)")
  (time (mm/r-lfw! :f))
  (println "(mm/r-sa! 'temp)")
  (time (mm/r-sa! 'temp))
  (println "(mm/r-lf! (pro/add-metrics-to-rec $rec/temp) :f)")
  (time (mm/r-lf! (pro/add-metrics-to-rec @(ns-resolve '$rec 'temp)) :f))
  (println "(time (mm/r-sa! rec-sym))")
  (time (mm/r-sa! rec-sym))
  (println "done"))


(defn trace-and-prof-querying-go-deep
  [n rec-sym]
  (trace-go-deep n 'dummy)
  (mm/w-rat!)
  (mm/w-atn! 'com.billpiel.mem-tracer.query)
  (mm/w-atn! 'com.billpiel.mem-tracer.util.tree-query)
  (println (mm/ws-show-traced))

  (println "(def temp-qr (mm/qw [depth #{3 4}]))")
  (time (def temp-qr (mm/qw [:depth #{3 4}])))

  (println "(mm/r-lfw!)")
  (time (mm/r-lfw! :f))
  (println "(mm/r-sa! 'temp)")
  (time (mm/r-sa! 'temp))
  (println "(mm/r-lf! (pro/add-metrics-to-rec $rec/temp) :f)")
  (time (mm/r-lf! (pro/add-metrics-to-rec @(ns-resolve '$rec 'temp)) :f))
  (println "(time (mm/r-sa! rec-sym))")
  (time (mm/r-sa! rec-sym))
  (println "done"))

(defn trace-and-prof-querying2-go-deep
  [n rec-sym]
  (trace-go-deep n 'dummy)
  (mm/w-rat!)
  (mm/w-atn! 'com.billpiel.mem-tracer.query2)
  (mm/w-atn! 'com.billpiel.mem-tracer.util.tree-query)
  (println (mm/ws-show-traced))
  (println "(def temp-qr (q2/query $rec/dummy #(-> % :depth #{3}))")
  (time (def temp-qr (q2/query @(ns-resolve '$rec 'dummy)
                               #(-> %
                                    :depth
                                    #{3 4}))))
  (println "(mm/r-lfw!)")
  (time (mm/r-lfw! :f))
  (println "(mm/r-sa! 'temp)")
  (time (mm/r-sa! 'temp))
  (println "(mm/r-lf! (pro/add-metrics-to-rec $rec/temp) :f)")
  (time (mm/r-lf! (pro/add-metrics-to-rec @(ns-resolve '$rec 'temp)) :f))
  (println "(time (mm/r-sa! rec-sym))")
  (time (mm/r-sa! rec-sym))
  (println "done"))


(defn pp-sort-fn-metrics
  [sort-key rec]
  (clojure.pprint/pprint (sort-by (comp sort-key
                                        second)
                                  (:fn-metrics rec))))

(defn ns-unmap-all
  [ns']
  (->> ns'
       ns-interns
       keys
       (map (partial ns-unmap ns'))
       dorun))
