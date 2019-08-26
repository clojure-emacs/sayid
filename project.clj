(defproject com.billpiel/sayid "0.0.18-SNAPSHOT"
  :description "Sayid is a library for debugging and profiling clojure code."
  :url "http://clojure-emacs.github.io/sayid/"
  :scm {:name "git" :url "https://github.com/clojure-emacs/sayid"}
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies [[tamarin "0.1.2"]
                 [org.clojure/tools.reader "1.3.2"]
                 [org.clojure/tools.namespace "0.3.1"]]
  :exclusions [org.clojure/clojure] ; see versions matrix below

  :aot [com.billpiel.sayid.sayid-multifn]

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]

  :repl-options {:nrepl-middleware [com.billpiel.sayid.nrepl-middleware/wrap-sayid]}

  :profiles {;; Clojure versions matrix
             :provided {:dependencies [[org.clojure/clojure "1.10.1"]
                                       [org.clojure/clojure "1.10.1" :classifier "sources"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojure "1.8.0" :classifier "sources"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojure "1.9.0" :classifier "sources"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]
                                   [org.clojure/clojure "1.10.1" :classifier "sources"]]}

             :dev {:dependencies [[nrepl "0.6.0"]]}}

  :aliases {"test-all" ["with-profile" "+1.8:+1.9:+1.10" "test"]})
