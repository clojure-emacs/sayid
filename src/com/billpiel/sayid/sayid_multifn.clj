(ns com.billpiel.sayid.sayid-multifn
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
  [m]
  [["DUMMY" vector nil (clojure.lang.Var/create)] m])


(defn -invoke
  [this & args]
  (let [{:keys [original trace-dispatch-fn trace-method-fn]} (.state this)
        dispatch-fn (.-dispatchFn original)
        dispatch-val (trace-dispatch-fn dispatch-fn
                                        args)
        method (.getMethod original dispatch-val)]
    (trace-method-fn method
                     args)))

(defn -reset
  [this]
  (-> this .state :original .reset))

(defn -addMethod
  [this dispatch-val method]
  (.addMethod (:original (.state this))
              dispatch-val
              method))

(defn -removeMethod
  [this dispatch-val]
  (-> this .state :original (.removeMethod dispatch-val)))

(defn -preferMethod
  [this dispatch-val-x dispatch-val-y]
  (-> this .state :original (.preferMethod dispatch-val-x dispatch-val-y)))

(defn -getMethodTable
  [this]
  (-> this .state :original .getMethodTable))

(defn -getPreferTable
  [this]
  (-> this .state :original .getPreferTable))



#_ (compile 'com.billpiel.sayid.sayid-multifn)

#_ (import 'com.billpiel.sayid.SayidMultiFn)

#_ (import 'com.billpiel.sayid.sayid_multifn)

#_ (com.billpiel.sayid.SayidMultiFn.)
