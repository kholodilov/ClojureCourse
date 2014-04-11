(ns task02.query
  (:use [task02 helpers db])
  (:use [clojure.core.match :only (match)]))

(declare do-parse)


;; Функция выполняющая парсинг запроса переданного пользователем
;;
;; Синтаксис запроса:
;; SELECT table_name [WHERE column comp-op value] [ORDER BY column] [LIMIT N] [JOIN other_table ON left_column = right_column]
;;
;; - Имена колонок указываются в запросе как обычные имена, а не в виде keywords. В
;;   результате необходимо преобразовать в keywords
;; - Поддерживаемые операторы WHERE: =, !=, <, >, <=, >=
;; - Имя таблицы в JOIN указывается в виде строки - оно будет передано функции get-table для получения объекта
;; - Значение value может быть либо числом, либо строкой в одинарных кавычках ('test').
;;   Необходимо вернуть данные соответствующего типа, удалив одинарные кавычки для строки.
;;
;; - Ключевые слова --> case-insensitive
;;
;; Функция должна вернуть последовательность со следующей структурой:
;;  - имя таблицы в виде строки
;;  - остальные параметры которые будут переданы select
;;
;; Если запрос нельзя распарсить, то вернуть nil

;; Примеры вызова:
;; > (parse-select "select student")
;; ("student")
;; > (parse-select "select student where id = 10")
;; ("student" :where #<function>)
;; > (parse-select "select student where id = 10 limit 2")
;; ("student" :where #<function> :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2")
;; ("student" :where #<function> :order-by :id :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2 join subject on id = sid")
;; ("student" :where #<function> :order-by :id :limit 2 :joins [[:id "subject" :sid]])
;; > (parse-select "werfwefw")
;; nil
(defn parse-query [^String query]
  (let [query-vec (vec (.split query " "))]
    (do-parse query-vec)))

(defn normalize-value [value]
  (match (re-matches #"'(.*)'" value)
    [_ str]
      str
    :else
      (parse-int value)))

(defn make-where-function [column comp-op value]
  (let [comp-op-norm (if (= comp-op "!=") "not=" comp-op)
        value-norm (normalize-value value)
        func (resolve (symbol comp-op-norm))
        col (keyword column)]
    (fn [data] (func (col data) value-norm))))

(defn do-parse [query-vec]
  (match query-vec
    ["select" tb & rest]
      (list* "select" tb (do-parse rest))
    ["delete" tb & rest]
      (list* "delete" tb (do-parse rest))
    ["update" tb & rest]
      (list* "update" tb (do-parse rest))
    ["insert" "into" tb & rest]
      (list "insert" tb (do-parse rest))
    ["set" column "=" value & rest]
      (list* {(keyword column) (normalize-value value)} (do-parse rest))
    ["with" column "=" value & rest]
      (merge {(keyword column) (normalize-value value)} (do-parse rest))
    ["where" column comp-op value & rest]
      (list* :where (make-where-function column comp-op value) (do-parse rest))
    ["order" "by" column & rest]
      (list* :order-by (keyword column) (do-parse rest))
    ["limit" n & rest]
      (list* :limit (parse-int n) (do-parse rest))
    ["join" tb "on" left-col "=" right-col]
      (list :joins [[(keyword left-col) tb (keyword right-col)]])
    []
      (list)
    :else nil))

;; Выполняет запрос переданный в строке.  Бросает исключение если не удалось распарсить запрос

;; Примеры вызова:
;; > (perform-query "select student")
;; ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "select student order by year")
;; ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})
;; > (perform-query "select student where id > 1")
;; ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "not valid")
;; exception...
(defn perform-query [^String query-string]
  (if-let [query (parse-query query-string)]
    (let [op (first query)
          table (first (rest query))
          args (rest (rest query))]
      (apply (ns-resolve 'task02.db (symbol op)) (get-table table) args))
    (throw (IllegalArgumentException. (str "Can't parse query: " query-string)))))
