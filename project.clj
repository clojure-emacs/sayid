(defproject mx.cider/sayid "0.3.0"
  :description "Sayid is a library for debugging and profiling clojure code."
  :url "https://github.com/clojure-emacs/sayid"
  :scm {:name "git" :url "https://github.com/clojure-emacs/sayid"}
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  ;; Clojure itself is supplied via the `:provided` profile (and overridden by
  ;; the version-matrix profiles below), so we exclude the transitive copies
  ;; pulled in by our dependencies to avoid version conflicts.
  :dependencies [[tamarin "0.1.2" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.reader "1.6.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.namespace "1.5.1" :exclusions [org.clojure/clojure]]]

  :aot [com.billpiel.sayid.sayid-multifn]

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]

  :repl-options {:nrepl-middleware [com.billpiel.sayid.nrepl-middleware/wrap-sayid]}

  :pedantic? :warn

  :profiles {;; Clojure is "provided" so that downstream projects bring their
             ;; own version. The version-matrix profiles below override it.
             :provided {:dependencies [[org.clojure/clojure "1.12.1"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.4"]]}
             :1.12 {:dependencies [[org.clojure/clojure "1.12.1"]]}

             :dev {:dependencies [[nrepl "1.3.1"]]}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2026.01.19"]]}}

  :aliases {"test-all" ["with-profile" "+1.10:+1.11:+1.12" "test"]})
