(ns sayid.plugin)

(def version
  "The current version of sayid as a string."
  "0.8.0")

(defn middleware
  [project]
  (-> project
      (update-in [:dependencies]
                 (fnil into [])
                 [['mx.cider/sayid version]])
      (update-in [:repl-options :nrepl-middleware]
                 (fnil into [])
                 ['sayid.nrepl-middleware/wrap-sayid])))
