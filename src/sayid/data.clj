(ns sayid.data
  "The recorded execution as plain, navigable Clojure data - the programmatic
  counterpart to the nREPL data ops.

  Where the nREPL data ops project the call tree for the wire (string keys,
  `pr-str`'d values, so it round-trips over bencode), this projects it for
  *scripting*: keyword keys and the captured values kept live, so you can walk,
  filter, and drill into them with ordinary Clojure - or hand the whole thing to a
  data-inspection tool.

  Because it's just `tap>`-able data, exploring a trace in
  [Portal](https://github.com/djblue/portal), Reveal or Morse is a one-liner:

      (require '[sayid.core :as sd] '[sayid.data :as sd-data])
      (sd/ws-add-trace-ns! my.ns)
      (my.ns/run)
      (sd-data/tap-trace!)   ; now explore it in Portal"
  (:require [sayid.core :as sd]))

(defn- duration
  "The call's wall-clock duration in ms, when both timestamps are present."
  [{:keys [started-at ended-at]}]
  (when (and started-at ended-at)
    (- ended-at started-at)))

(defn node->data
  "Project one recorded node onto plain data: its function or expression, the
  captured argument and return (or throw) values kept *live*, its timing and
  source location, and its children.  Internal bookkeeping (paths, depth, raw
  timestamps, var metadata) is dropped."
  [node]
  (let [m (:meta node)]
    (cond-> {:id (:id node)
             :name (:name node)
             :children (mapv node->data (:children node))}
      (contains? node :args)    (assoc :args (:args node))
      (:arg-map node)           (assoc :arg-map (:arg-map node))
      (:form node)              (assoc :form (:form node))
      (:inner-tags node)        (assoc :inner-tags (:inner-tags node))
      (contains? node :return)  (assoc :return (:return node))
      (:throw node)             (assoc :throw (:throw node))
      (duration node)           (assoc :ms (duration node))
      (:line m)                 (assoc :source (select-keys m [:file :line])))))

(defn trace-data
  "The active workspace's recorded call trees (or workspace W's) as plain,
  navigable Clojure data - a vector of root calls, each with live captured
  values.  This is the scripting entry point: `(->> (trace-data) (mapcat ...) ...)`."
  [& [w]]
  (mapv node->data (:children (if w (sd/ws-deref! w) (sd/ws-deref!)))))

(defn tap-trace!
  "`tap>` the recorded trace as data, for exploring in Portal / Reveal / Morse.
  Returns the number of root calls tapped."
  [& [w]]
  (let [d (trace-data w)]
    (tap> d)
    (count d)))
