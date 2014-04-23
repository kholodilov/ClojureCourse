(defproject webapp-task "0.1.0-SNAPSHOT"
  :dependencies [
    [org.clojure/clojure "1.6.0"]
    [ring "1.2.2"]
  ]
  :plugins [[lein-ring "0.8.10"]]
  :ring {:handler webapp.core/app})
