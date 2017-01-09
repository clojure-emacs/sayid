(ns com.billpiel.sayid.sayid-multifn
  (:require [com.billpiel.sayid.trace :as t])
  (:gen-class :name com.billpiel.sayid.SayidMultiFn
              :init init
              :constructors {[clojure.lang.IPersistentMap
                              String
                              clojure.lang.IFn
                              Object
                              clojure.lang.IRef]
                             [String
                              clojure.lang.IFn
                              Object
                              clojure.lang.IRef]}
              :extends clojure.lang.MultiFn
              :prefix "-"
              :state state))

(defn -init
  [m n dispatch-fn default r]
  [[n dispatch-fn default r] m])

(defn -addMethod
  [this dispatch-val method]
  (.addMethod (:original (.state this))
              dispatch-val
              method))

(defn -invoke
  [this & args]
  (let [{:keys [workspace name' meta' original]} (.state this)
        dispatch-fn (.-dispatchFn original)
        dispatch-val (t/trace-fn-call workspace
                                      (symbol (str name' "--DISPATCHER"))
                                      dispatch-fn
                                      args
                                      meta')
        method (.getMethod original dispatch-val)]
    (t/trace-fn-call workspace
                     name'
                     method
                     args
                     meta')))

#_ (compile 'com.billpiel.sayid.sayid-multifn)

#_ (import 'com.billpiel.sayid.SayidMultiFn)

#_ (import 'com.billpiel.sayid.sayid_multifn)

#_ (com.billpiel.sayid.SayidMultiFn.)
