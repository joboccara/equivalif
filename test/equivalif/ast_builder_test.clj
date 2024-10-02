(ns equivalif.ast-builder-test
 (:require [clojure.test :refer :all]
           [equivalif.ast-builder :refer :all]))

(deftest and-expression
  (testing "Returns a list representing the boolean expression"
    (is (= '(and a b) (parse "a && b")))))

(deftest or-expression
  (testing
    (is (= '(or a b) (parse "a || b")))))

(deftest not-expression
  (testing
   (is (= '(not a) (parse "!a")))))

(deftest single-symbol
  (testing
   (is (= 'a (parse "a")))))

(deftest nested-expressions
  (testing
   (is (= '(and (or a (not (and b c))) (or a (and c d))) (parse "(a || (!(b && c))) && (a || (c && d))")))))
