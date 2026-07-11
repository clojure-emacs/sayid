(ns sayid.shelf
  (:require [sayid.util.other :as util]
            [sayid.util.sym :as sym]))


(defn save!
  "Shelve ITEM's value under its SLOT-KW slot in namespace SHELF.  PREP, when
  given, transforms the value before it's stored - the workspace shelf uses this
  to store an independent snapshot rather than a live, still-mutating value."
  [item shelf slot-kw mk-ex-msg-fn & [prep]]
  (let [item' ((or prep identity) @item)
        slot (slot-kw item')]
    (if (symbol? slot)
      (sym/def-ns-var shelf slot item')
      (throw (Exception. ^String (mk-ex-msg-fn
                                  slot))))
    item'))

(defn save-as!
  [item shelf slot-kw slot mk-ex-msg-fn & [prep]]
  (swap! item assoc slot-kw (sym/qualify-sym shelf slot))
  (save! item shelf slot-kw mk-ex-msg-fn prep)
  item)

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
    (throw (Exception. ^String load-over-unsaved-ex-msg))))

#_ (defn load!
     [item shelf slot load-over-unsaved-ex-msg & [force]]
     (if (safe-to-load? item shelf force)
       (reset! item @(ns-resolve shelf slot))
       (throw (Exception. load-over-unsaved-ex-msg))))
