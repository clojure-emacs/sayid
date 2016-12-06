(ns com.billpiel.sayid.test-utils)

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

(let [[f & r] (range 1 100)] [f r])


(defn make-mock-series-lazy-fn
  [f s]
  (let [a (atom s)]
    (fn [& args]
      (let [v (-> a
                  (swap-pair! rest)
                  first
                  first)]
        (apply f (into [v] args))))))

(def mock-now-fn #(make-mock-series-fn identity
                                       (vec (range 0 1000))))

(def mock-gensym-fn (fn []
                      (make-mock-series-fn
                       (fn [id & [pre]]
                         (str (or pre "") id))
                       (vec (map str (range 10 1000))))))

(defn remove-iso-ctrl [s]  (apply str (remove #(Character/isISOControl %) s)))

(def ansi-colors [:black :red :green :yellow :blue :magenta :cyan :white])

(defn kw->bg [kw] (->> kw name (str "bg-") keyword))

(defn ansi->kw
  [a]
  (try (let [a' (if (string? a)
                  (Integer/parseInt a)
                  a)]
         (cond (= a' 0) :bold-off
               (= a' 1) :bold
               (<= 30 a' 39) (nth ansi-colors (mod a' 10))
               (<= 40 a' 49) (->> (mod a' 10)
                                  (nth ansi-colors)
                                  kw->bg)))
       (catch Exception ex nil)))

(defn replace-ansi*
  [s coll]
  (if-let [[both text code] (re-find (re-pattern (format "(?is)^(.*?)(%s\\[[\\d;]*m)" \u001B))
                                     s)]
    (do  [s both (count both) text (count text) code (count code)]
        (recur (subs  s (count both))
               (into coll [text code])))
    (into coll [s])))

(defn tag-ansi
  [s]
  (if-let [[_ code] (re-find (re-pattern (format "%s\\[([\\d;]*)m" \u001B))
                             s)]
    (let [codes (clojure.string/split code #";")]
      (mapv ansi->kw codes))
    s))

(defn replace-ansi
  [s]
  (let [v (replace-ansi* s [])]
    (mapv tag-ansi v)))

(defn redact-file-fn
  [& paths]
  (fn [v]
    (loop [v v
           [f & r] paths]
      (if (nil? f)
        v
        (recur (update-in v f (constantly "FILE"))
               r)))))
