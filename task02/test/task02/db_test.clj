(ns task02.db-test
  (:require [task02.db :refer :all]
            [task02.query :as q]
            [clojure.test :refer :all]))

(load-initial-data)

(deftest insert-test
  (load-initial-data)
  (testing "insertion..."
    (insert student {:id 10 :surname "Test" :year 2000})
    (let [rs (q/perform-query "select student where id = 10")]
      (is (not (empty? rs)))
      (is (= (count rs) 1))
      (is (= (:year (first rs)) 2000)))
    ))

(deftest delete-test
  (load-initial-data)
  (testing "deletion..."
    (q/perform-query "delete student where id = 1")
    (let [rs (q/perform-query "select student where id = 1")]
      (is (empty? rs)))
      (is (= (count (q/perform-query "select student")) 2))
    ))

(deftest delete-all-test
  (load-initial-data)
  (testing "total deletion..."
    (q/perform-query "delete student")
    (let [rs (q/perform-query "select student")]
      (is (empty? rs)))))

(deftest update-test
  (load-initial-data)
  (testing "update..."
    (let [rs (q/perform-query "select student where id = 1")]
      (is (not (empty? rs)))
      (is (= (count rs) 1))
      (is (= (:year (first rs)) 1998))
      )
    (q/perform-query "update student set year = 2000 where id = 1")
    (let [rs (q/perform-query "select student")]
      (is (= (count rs) 3))
      )
    (let [rs (q/perform-query "select student where id = 1")]
      (is (not (empty? rs)))
      (is (= (count rs) 1))
      (is (= (:year (first rs)) 2000))
      )
    ))

(deftest update-all-test
  (load-initial-data)
  (testing "total update..."
    (q/perform-query "update student set year = 2000")
    (let [rs (q/perform-query "select student where year = 2000")]
      (is (= (count rs) 3))
      )
    ))
