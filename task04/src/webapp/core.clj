(ns webapp.core
  (:use ring.adapter.jetty
        ring.util.response
        ring.middleware.session))

(defn handler [request]
  (let [cnt (or (:req-count (:session request)) 0)]
    (-> (response (str cnt " requests\n\n" request))
      (assoc :session {:req-count (inc cnt)}))))

(def app
  (-> handler
    wrap-session))