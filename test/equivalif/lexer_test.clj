(ns equivalif.lexer-test
 (:require [clojure.test :refer :all]
            [equivalif.lexer :refer :all]))

(deftest empty-source
  (testing "Empty source returns empty array"
   (is (= [] (lex "")))))

(deftest variable-token
  (testing "Symbol returns variable token"
    (is (= [{:type :variable, :name "value1"}] (lex "value1")))))

(deftest &&-token
  (testing "&& returns and token"
    (is (= [{:type :variable, :name "a"},
           {:type :and}
           {:type :variable, :name "b"}]
           (lex "a && b")))))
