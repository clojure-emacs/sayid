(ns com.billpiel.mem-tracer.test.manual
  (:require [com.billpiel.mem-tracer.core :as mm]
            [com.billpiel.mem-tracer.profiling :as pro]))

(defn go-deep [n & r]
  (if (> n 0)
    (->> n
         (range 0)
         (map #(concat [n %]
                       (range 0 n)))
         (mapv #(apply go-deep (dec n) %))
         flatten
         (apply +))
    r))

(defn trace-go-deep
  [n rec-sym]
  (mm/ws-reset!)
  (mm/ws-add-trace-ns! com.billpiel.mem-tracer.test.manual)
  (go-deep n)
  (mm/ws-remove-all-traces!)
  (mm/rec-load-from-ws! :f)
  (mm/rec-save-as! rec-sym))

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
  (mm/w-cl!
   )
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
