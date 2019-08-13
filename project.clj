(defproject com.billpiel/sayid "0.0.17"
  :description "Sayid is a library for debugging and profiling clojure code."
  :url "http://clojure-emacs.github.io/sayid/"
  :scm {:name "git" :url "https://github.com/clojure-emacs/sayid"}
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :aot [com.billpiel.sayid.sayid-multifn]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [tamarin "0.1.2"]
                 [org.clojure/tools.reader "1.3.0-alpha3"]
                 [org.clojure/tools.namespace "0.2.11"]]
  :repl-options {:nrepl-middleware [com.billpiel.sayid.nrepl-middleware/wrap-sayid]}
  :profiles {:dev {:dependencies [[nrepl "0.4.4"]]
                   :plugins [[lein-codox "0.9.4"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}}
  :codox {:project {:name "Sayid"}
          :namespaces [com.billpiel.sayid.core]}
  :aliases {"test-all" ["with-profile" "+1.8:+1.9:+1.10" "test"]})
