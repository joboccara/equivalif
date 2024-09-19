(ns equivalif.lexer-test
 (:require [clojure.test :refer :all]
            [equivalif.lexer :refer :all]))

(deftest empty-source
  (testing "Empty source returns empty array"
   (is (= [] (lex "")))))

(deftest one-variable
  (testing "Symbol returns variable token"
    (is (= [{:type :variable, :value "value1"}] (lex "value1")))))
