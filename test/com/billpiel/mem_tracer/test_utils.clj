(ns com.billpiel.mem-tracer.test-utils)

;; https://github.com/Prismatic/plumbing/blob/6f9f1b6453ed2c978a619dc99bb0317d8c053141/src/plumbing/core.cljx#L356
(defn swap-pair!
  "Like swap! but returns a pair [old-val new-val]"
  ([a f]
     (loop []
       (let [old-val @a
             new-val (f old-val)]
         (if (compare-and-set! a old-val new-val)
           [old-val new-val]
           (recur)))))
  ([a f & args]
   (swap-pair! a #(apply f % args))))

(defn make-mock-series-fn
  [f s]
  (let [a (atom s)]
    (fn [& args]
      (let [v (-> a
                  (swap-pair! subvec 1)
                  first
                  first)]
        (apply f (into [v] args))))))

(def mock-now-fn #(make-mock-series-fn identity
                                       [#inst "2010-01-01T01:00:00.000-00:00"
                                        #inst "2010-01-01T02:00:00.000-00:00"
                                        #inst "2010-01-01T03:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"
                                        #inst "2010-01-01T04:00:00.000-00:00"]))

(def mock-gensym-fn (fn []
                      (make-mock-series-fn
                       (fn [id & [pre]]
                         (str (or pre "") id))
                       (vec (map str (range 10 1000))))))

(defn remove-iso-ctrl [s]  (apply str (remove #(Character/isISOControl %) s)))

(defn replace-iso-ctrl [s r]  (apply str (map #(if (Character/isISOControl %)
                                              r
                                              %) s)))

(replace-iso-ctrl "hello\33[31m;whoa" "~~~")

(defn replace-ansi*
  [s coll]
  #spy/d s
  (if-let [[both text code] #spy/d (re-find #"(.*?)(~#~\[[\d*;?]*m)"
                                            (replace-iso-ctrl s "~#~"))]
    (recur (subs s  (-> both count (- 2)))
           (into coll [text code]))
    (into coll [s])))

(defn tag-ansi
  [s]
  (if-let [[_ code] (re-find #"~#~\[(.*?)m"
                             s)]
    (let [codes (clojure.string/split code #";")]
      (mapv {"31" :red "41" :bg-red "1" :bold} codes)) ;; TODO #->color-kw converter
    s))

(let [re #"~#~\[(.*?)m"]

  [
   (tag-ansi "~#~[31m")
   (tag-ansi "~#~[31;41m")
   (tag-ansi "~#~[1;31;41m")]

)


(replace-ansi* "hello\33[31mwhoa" [])

(replace-ansi* "hello\33[31mwhoahello\33[31mwhoa" [])



(defn replace-ansi
  [s]
  (let [v (replace-ansi* s [])]
    (mapv tag-ansi v)))

(replace-ansi "hello\33[31mwhoahello\33[1;31;41mwhoa")


(re-find
 (re-pattern (str \u001B "\\d*;m"))
 "\33[31m;whoa")
