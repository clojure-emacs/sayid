(ns com.billpiel.mem-tracer.shelf
  (:require [com.billpiel.mem-tracer.util.other :as util]))


(defn save!
  [item shelf slot-kw mk-ex-msg-fn]
  (let [item' @item
        slot (slot-kw item')]
    (if (symbol? slot)
      (util/def-ns-var shelf slot item')
      (throw (Exception. (mk-ex-msg-fn
                          slot))))
    item'))

(defn save-as!
  [item shelf slot-kw slot mk-ex-msg-fn]
  (doto item
    (swap! assoc slot-kw
           (util/qualify-sym shelf slot))
    (save! shelf slot-kw mk-ex-msg-fn)))

(defn safe-to-load?
  [item shelf slot-kw & [force]]
  (let [item' @item]
    (or (= :f force)
        (nil? item')
        (some->> item'
                 slot-kw
                 (ns-resolve shelf)))))

(defn load!
  [item shelf slot-kw slot-src load-over-unsaved-ex-msg & [force]]
  (if (safe-to-load? item
                     shelf
                     slot-kw
                     force)
    (let [source (util/just-get-whatever-you-can shelf
                                                 slot-src)]
      (reset! item source))
    (throw (Exception. load-over-unsaved-ex-msg))))

#_ (defn load!
     [item shelf slot load-over-unsaved-ex-msg & [force]]
     (if (safe-to-load? item shelf force)
       (reset! item @(ns-resolve shelf slot))
       (throw (Exception. load-over-unsaved-ex-msg))))
