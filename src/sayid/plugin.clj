(ns sayid.plugin
  (:require
   [clojure.java.io :as io]))

(def version
  "The current version of sayid as a string."
  (-> (or (io/resource  "META-INF/leiningen/com.billpiel/sayid/project.clj")
          "project.clj")
      slurp
      read-string
      (nth 2)))

(defn middleware
  [project]
  (-> project
      (update-in [:dependencies]
                 (fnil into [])
                 [['com.billpiel/sayid version]])
      (update-in [:repl-options :nrepl-middleware]
                 (fnil into [])
                 ['com.billpiel.sayid.nrepl-middleware/wrap-sayid])))
