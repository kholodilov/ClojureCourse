(ns task02.query-test
  (:require [clojure.test :refer :all]
            [task02.query :refer :all]
            [task02.db :as db]
            ))


(deftest parse-select-test
  (testing (str "parse-select on 'select student'")
    (let [[op tb-name & {:keys [where limit order-by joins]}]
          (parse-query "select student")]
      (is (= op "select"))
      (is (= tb-name "student"))
      (is (nil? where))
      (is (nil? order-by))
      (is (nil? joins))
      (is (nil? limit))))

  (testing (str "parse-select on 'select student where id = 10'")
    (let [[op tb-name & {:keys [where limit order-by joins]}]
          (parse-query "select student where id = 10")]
      (is (= tb-name "student"))
      (is (fn? where))
      (is (nil? order-by))
      (is (nil? joins))
      (is (nil? limit))))

  (testing (str "parse-select on 'select student where id = 10 order by year limit 5 join subject on id = sid'")
    (let [[op tb-name & {:keys [where limit order-by joins]}]
          (parse-query "select student where id = 10 order by year limit 5 join subject on id = sid")]
      (is (= op "select"))
      (is (= tb-name "student"))
      (is (fn? where))
      (is (= order-by :year))
      (is (= limit 5))
      (is (= joins [[:id "subject" :sid]]))))
  )

(deftest parse-delete-test
  (testing "parse-delete with where clause"
    (let [[op tb-name & {:keys [where]}]
          (parse-query "delete student where id = 10")]
      (is (= op "delete"))
      (is (= tb-name "student"))
      (is (fn? where))))
  (testing "parse-delete without where clause"
    (let [[op tb-name & {:keys [where]}]
          (parse-query "delete student")]
      (is (= op "delete"))
      (is (= tb-name "student"))
      (is (nil? where)))))

(deftest parse-update-test
  (testing "parse-update with where clause"
    (let [[op tb-name upd-map & {:keys [where]}]
          (parse-query "update student set year = 2000 where id = 10")]
      (is (= op "update"))
      (is (= tb-name "student"))
      (is (= upd-map {:year 2000}))
      (is (fn? where))))
  (testing "parse-update without where clause"
    (let [[op tb-name upd-map & {:keys [where]}]
          (parse-query "update student set year = 2000")]
      (is (= op "update"))
      (is (= tb-name "student"))
      (is (= upd-map {:year 2000}))
      (is (nil? where)))))

(deftest perform-query-select-test
  (db/load-initial-data)
  (testing "perform-query select"
      (is (= (perform-query "select student where year = 1997")
            '({:year 1997, :surname "Petrov", :id 2})))
      (is (= (perform-query "select student where surname = 'Sidorov'")
            '({:year 1996, :surname "Sidorov", :id 3})))
      (is (= (perform-query "select student where year = 1111")
            '()))))
