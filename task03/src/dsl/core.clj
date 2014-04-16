(ns dsl.core
  (:use clojure.walk))

(def cal (java.util.Calendar/getInstance))
(def today (java.util.Date.))
(def yesterday (do (.add cal java.util.Calendar/DATE -1) (.getTime cal)))
(def tomorrow (do (.add cal java.util.Calendar/DATE 2) (.getTime cal)))

(comment
  (defn one [] 1)
  
  ;; Примеры вызова
  (with-datetime
    (if (> today tomorrow) (println "Time goes wrong"))
    (if (<= yesterday today) (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly")))))


;; Режим Бога -- никаких подсказок.
;; Вы его сами выбрали ;-)
(defmacro with-datetime [& code]
  `(let [~(symbol ">") (fn [d1# d2#] (> (.compareTo d1# d2#) 0))
         ~(symbol "<") (fn [d1# d2#] (< (.compareTo d1# d2#) 0))
         ~(symbol "<=") (fn [d1# d2#] (<= (.compareTo d1# d2#) 0))
         ~(symbol ">=") (fn [d1# d2#] (>= (.compareTo d1# d2#) 0))
         ~(symbol "day") '()
         ~(symbol "days") '()
         ~(symbol "week") '()
         ~(symbol "weeks") '()
         ~(symbol "month") '()
         ~(symbol "months") '()
         ~(symbol "year") '()
         ~(symbol "years") '()]
      ~@code))

(comment
  (macroexpand-1
    '(with-datetime
      (if (> today tomorrow) (println "Time goes wrong") (println "ok"))))

  (with-datetime
    (if (> today tomorrow) (println "Time goes wrong") (println "Correct"))
    (if (<= yesterday today) (println "Correct") (println "Time goes wrong")))
)
