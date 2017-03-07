(ns sayid.plugin)

(defn middleware
  [project]
  (-> project
      (update-in [:dependencies]
                 (fnil into [])
                 [['com.billpiel/sayid "0.0.14"]])
      (update-in [:repl-options :nrepl-middleware]
                 (fnil into [])
                 ['com.billpiel.sayid.nrepl-middleware/wrap-sayid])))
