(ns dsl.core
  (:use [clojure.walk :only (prewalk)])
  (:use [clojure.core.match :only (match)])
  ;(:import (org.apache.commons.lang3.time DateUtils))
  )

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
        (println "DSL works correctly"))))
 )

(def date-symbols
  {
    'day     'org.apache.commons.lang3.time.DateUtils/addDays
    'days    'org.apache.commons.lang3.time.DateUtils/addDays
    'week    'org.apache.commons.lang3.time.DateUtils/addWeeks
    'weeks   'org.apache.commons.lang3.time.DateUtils/addWeeks
    'month   'org.apache.commons.lang3.time.DateUtils/addMonths
    'months  'org.apache.commons.lang3.time.DateUtils/addMonths
    'year    'org.apache.commons.lang3.time.DateUtils/addYears
    'years   'org.apache.commons.lang3.time.DateUtils/addYears
  })

(defn date-symbol? [sym] (and (symbol? sym) (contains? date-symbols sym)))

(defmacro with-datetime [& code]
  (let [transformed-code
         (prewalk
           (fn [form]
             (if (list? form)
               (match (vec form)
                 [date op n (sym :guard date-symbol?)]
                   (let [transform-fn (sym date-symbols)
                         op-str (str op)]
                     `(~transform-fn ~date (Integer/parseInt (str ~op-str ~n))))
                 :else form)
               form))
           code)]
  `(let [~(symbol ">") (fn [d1# d2#] (> (.compareTo d1# d2#) 0))
         ~(symbol "<") (fn [d1# d2#] (< (.compareTo d1# d2#) 0))
         ~(symbol "<=") (fn [d1# d2#] (<= (.compareTo d1# d2#) 0))
         ~(symbol ">=") (fn [d1# d2#] (>= (.compareTo d1# d2#) 0))]
      ~@transformed-code)))