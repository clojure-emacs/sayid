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
  [m n dispatch-fn default r]
  [[n dispatch-fn default r] m])



#_ (compile 'com.billpiel.sayid.sayid-multifn)

#_ (import 'com.billpiel.sayid.SayidMultiFn)

#_ (import 'com.billpiel.sayid.sayid_multifn)

#_ (com.billpiel.sayid.SayidMultiFn.)
