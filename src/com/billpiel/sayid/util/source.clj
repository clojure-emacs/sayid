(ns com.billpiel.sayid.util.source
  "Helpers for locating and reading the source of a function, used by inner
  tracing to recover the form it needs to instrument."
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rts]
            clojure.repl
            clojure.string
            clojure.java.io))

(defn mk-dummy-whitespace
  [lines cols]
  (apply str
         (concat (repeat lines "\n")
                 (repeat cols " "))))

(defn mk-positionalble-src-logging-push-back-rdr
  [s file line col]
  (rts/source-logging-push-back-reader (str (mk-dummy-whitespace (dec line) ;;this seem unfortunate
                                                                 (dec col))
                                            s)
                                       (+ (count s) line col 1)
                                       file))

(defn hunt-down-source
  [fn-sym]
  (let [{:keys [source file line column]} (-> fn-sym
                                              resolve
                                              meta)]
    (or source
        (r/read (mk-positionalble-src-logging-push-back-rdr
                 (or
                  (clojure.repl/source-fn fn-sym)
                  (->> file
                       slurp
                       clojure.string/split-lines
                       (drop (dec line))
                       (clojure.string/join "\n"))
                  "nil")
                 file
                 line
                 column)))))

(defn get-src-file-path
  [s]
  (let [s' (clojure.string/replace s #"^file:" "")]
    (if (.exists (java.io.File. s'))
      s'
      (when-let [r (clojure.java.io/resource s')]
        (.getPath r)))))
