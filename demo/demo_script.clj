(ns demo-script)

;; ### quick start

(require '[com.billpiel.sayid.core :as sd])

sd/src-in-meta
(defn func-example1 [a b]
 (* a b))

sd/src-in-meta
(defn func-example2
  [a b]
  (if (< a b)
    (-> a
        inc
        (func-example1 b)
        (* 2)
        (vector a b))
    (recur a (inc b))))

(sd/ws-add-deep-trace-fn! func-example2)

(func-example2 3 1)

(sd/ws-add-deep-trace-fn! func-example2)

sd/src-in-meta
(defn func-example2
  [a b]
  (if (< a b)
    (-> a
        dec ;; was inc
        (func-example1 b)
        (* 3) ;; was (* 2)
        (vector a b))
    (recur a (inc b))))

(sd/w-$ [:name func-example2])


;; ### save this for querying

(sd/q inc)

(sd/q :a inc)

(sq/q [:return second 3])


;; ### workspace

{:id :root22440          ;; unique id
 :path [:root22440]      ;; unique path
 :depth 0                ;; depth from root of trace tree
 :children (atom [])     ;; top-level function traces will go here
 :traced {:ns #{com.billpiel.sayid.test.ns1} ;; this ns is traced
          :fn #{}           ;; no invididually traced functions
          :deep-fn #{}}     ;; no deep traced functions
 :ws-slot nil               ;; workspace has not been saved in the shelf
 :arg-map nil}              ;; not applicable


>>>>>>> Stashed changes
(doc ws-save-as!)

(doc ws-load!)

(sd/ws-save-as! :demo)

;; use tab completion to show contents of $ws

;; ### querying

(doc query-docs)

;; get parent and child
(sd/q :ad 1 [:return 3])

;; getting a value
(def ret (-> (sd/qw func)
             first
             :return))

;; ### profiling

(doc sd/pro-net-time)

(doc sd/pro-gross-repeats)

(def ppp (sd/pro-analyse (sd/ws-deref!)))

(sd/pro-gross-repeats ppp)


;; ### recordings

(sd/rec-load-from-ws! )
