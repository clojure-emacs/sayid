(ns sayid.util.sym
  "Helpers for working with symbols and namespaces: qualifying and resolving
  symbols, and evaluating or macroexpanding forms in a given namespace."
  (:require [clojure.walk :as walk]))

(defn def-ns-var
  [ws-ns-sym sym v]
  (binding [*ns* (create-ns ws-ns-sym)]
    (eval `(def ~sym '~v))))

(defn eval-in-ns
  [ns-sym form]
  (binding [*ns* (create-ns ns-sym)]
    (use 'clojure.core)
    (eval form)))

(defn macroexpand-in-ns
  [ns-sym form]
  (eval-in-ns ns-sym `(macroexpand '~form)))

(defn macroexpand-all-in-ns
  [ns-sym form]
  (eval-in-ns ns-sym `(walk/macroexpand-all '~form)))

(defn macro?
  [ns-sym m-sym]
  (try (some->> m-sym
                (ns-resolve ns-sym)
                meta
                :macro)
       (catch Exception e false)))

(defn qualify-sym
  [ns sym]
  (symbol (str ns)
          (str sym)))

(defn disqualify-sym
  [fn-sym]
  (->> fn-sym
       str
       (re-find #"(.*?)/(.*)")
       rest
       (mapv symbol)))

(defmacro fully-qualify-sym
  [sym]
  `(let [m# (-> ~sym
                resolve
                meta)
         ns# (-> m# :ns str)
         name# (-> m# :name)]
     (qualify-sym ns# name#)))

(defn resolve-to-qual-sym [ns-sym sym]
  (try (when-let [{:keys [name ns]} (meta (ns-resolve ns-sym sym))]
         (qualify-sym ns name))
       (catch Exception e
         nil)))
