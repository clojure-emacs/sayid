(ns com.billpiel.sayid.nrepl-middleware
  (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as transport]
            [com.billpiel.sayid.core :as sd]))



(defn sayid-query-form-at-point
  [{:keys [transport file line] :as msg}]
  (transport/send transport (response-for msg
                                          :value (-> (sd/ws-query-by-file-pos file line)
                                                     sd/trees-print
                                                     with-out-str)))
  (transport/send transport (response-for msg :status :done)))

(def sayid-nrepl-ops
  {"sayid-query-form-at-point" #'sayid-query-form-at-point})

(defn wrap-sayid
  [handler]
  (fn [{:keys [op] :as msg}]
    ((get sayid-nrepl-ops op handler) msg)))

(set-descriptor! #'wrap-sayid
                 {:handles (zipmap (keys sayid-nrepl-ops)
                                   (repeat {:doc "See the sayid-nrepl README"
                                            :returns {} :requires {}}))})
