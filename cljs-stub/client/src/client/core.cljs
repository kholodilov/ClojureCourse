(ns client.core
  (:require [enfocus.core :as ef])
  (:require-macros [enfocus.macros :as em]))

(defn start []
  (ef/at "body" (ef/content "Hello world!")))


;; (set! (.-onload js/window) start)
(set! (.-onload js/window) #(em/wait-for-load (start)))

