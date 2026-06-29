(ns sayid.plugin)

(def version
  "The current version of sayid as a string."
  "0.3.0")

(defn middleware
  [project]
  (-> project
      (update-in [:dependencies]
                 (fnil into [])
                 [['mx.cider/sayid version]])
      (update-in [:repl-options :nrepl-middleware]
                 (fnil into [])
                 ['com.billpiel.sayid.nrepl-middleware/wrap-sayid])))
