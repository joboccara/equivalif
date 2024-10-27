(ns equivalif.collection-helpers-test
  (:require [clojure.test :refer [deftest is testing]]
            [equivalif.collection-helpers :refer [gsub remove-if-index]]))

(deftest gsub-inside-new-longer
  (testing (is (= '(1 a b c 4 5) (gsub '(1 2 3 4 5) '(2 3) '(a b c))))))

(deftest gsub-inside-new-shorter
  (testing (is (= '(1 a 4 5) (gsub '(1 2 3 4 5) '(2 3) '(a))))))

(deftest gsub-inside-new-same-size
  (testing (is (= '(1 a b 4 5) (gsub '(1 2 3 4 5) '(2 3) '(a b))))))

(deftest gsub-left-end
  (testing (is (= '(a b 4 5) (gsub '(1 2 3 4 5) '(1 2 3) '(a b))))))

(deftest gsub-right-end
  (testing (is (= '(1 2 3 a b) (gsub '(1 2 3 4 5) '(4 5) '(a b))))))

(deftest remove-if-index-even
  (testing (is (= '(10 30 50) (remove-if-index even? '(0 10 20 30 40 50))))))

(deftest remove-if-index-less-than-2
  (testing (is (= '(20 30 40 50) (remove-if-index #(< % 2) '(0 10 20 30 40 50))))))