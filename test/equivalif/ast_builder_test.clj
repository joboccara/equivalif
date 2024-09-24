(ns equivalif.ast-builder-test
 (:require [clojure.test :refer :all]
           [equivalif.ast-builder :refer :all]))

(deftest single-symbol
  (testing "Returns a list representing the boolean expression"
  (is (= '(a) (parse "a")))))

(deftest and-expression
  (testing
    (is (= '(and a b) (parse "a && b")))))

(deftest or-expression
  (testing
    (is (= '(or a b) (parse "a || b")))))
