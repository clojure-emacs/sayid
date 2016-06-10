(ns com.billpiel.sayid.view
  (:require [com.billpiel.sayid.util.other :as util]))


;; ==================================================================
;; These also exist in string-output. Move to some common location.


(defn get-some*
  [f v]
  (cond
    (fn? f)
    (f v)

    (set? f)
    (f v)

    :default
    (get v f)))

(defn get-some
  [coll v]
  (loop [coll coll
         v v]
    (if ((some-fn empty? nil?) coll)
      v
      (let [[f & r] coll]
        (when-let [v' (get-some* f v)]
          (recur r v'))))))

(defn wrap-wildcards
  [re]
  (re-pattern (str ".*" re ".*")))

(defn eq* [pred v]
  (cond (fn? pred)
        (pred v)

        (set? pred)
        (pred v)

        (instance? java.util.regex.Pattern pred)
        (->> v
             str
             (re-matches (wrap-wildcards pred)))

        (= pred (boolean pred)) ;; added this
        pred

        :default (= pred v)))

(defn mk-query-fn
  [query-coll]
  (let [path (drop-last query-coll)
        pred (last query-coll)]
    (fn [v]
      (try
        (->> v
             (get-some path)
             (eq* pred))
        (catch Exception ex
          nil)))))

(defn some-mk-query-fn
  [queries]
  (->> queries
       (map mk-query-fn)
       (apply some-fn)))

;; ==================================================================

(defn pred-sel-pairs->view
  [pairs]
  (let [pairs' (map (fn [[pred sel]]
                      [(-> pred
                           util/->vec
                           mk-query-fn)
                       sel])
                    pairs)]
    (->> pairs'
         (map first)
         (apply some-fn))))

(defn pred-sel-pair->pred-fn
  [[pred sel]]
  (fn [tree]
    (if ((-> pred
             util/->vec
             mk-query-fn)
         tree)
      (if (fn? sel)
        (sel tree)
        sel)
      nil)))

(defn pred-sel-pairs->view
  [pairs]
  (->> pairs
       (map pred-sel-pair->pred-fn)
       (apply some-fn)))

(defn mk-simple-view
  ([] (mk-simple-view {:args true
                       :return true
                       :children true
                       :throw true
                       :selects false}))
  ([selector] (pred-sel-pairs->view [[true selector]])))
