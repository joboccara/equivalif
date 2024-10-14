(ns equivalif.lexer-test
 (:require [clojure.test :refer [deftest is testing]]
            [equivalif.lexer :refer [lex]]))

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

(deftest ||-token
  (testing "|| returns or token"
    (is (= [{:type :variable, :name "a"},
           {:type :or}
           {:type :variable, :name "b"}]
           (lex "a || b")))))

(deftest !-token
  (testing "! returns not token"
    (is (= [{:type :not}, {:type :variable, :name "a"}]
           (lex "!a")))))

(deftest open-paren-token
  (testing "parentheses returns open or close tokens"
    (is (= [{:type :open},
            {:type :variable, :name "a"}]
           (lex "(a")))))

(deftest close-paren-token
  (testing "parentheses returns open or close tokens"
    (is (= [{:type :variable, :name "a"},
            {:type :close}]
           (lex "a)")))))

(deftest paren-tokens
  (testing "parentheses returns open or close tokens"
    (is (= [{:type :open},
            {:type :open},
            {:type :variable, :name "a"}
            {:type :close},
            {:type :open},
            {:type :variable, :name "b"},
            {:type :close},
            {:type :close}]
           (lex "((a) (b))")))))

(deftest function-calls-converted-to-variables
  (testing (and (is (= [{:type :variable, :name "f(x + 1)"}] (lex "f(x + 1)")))
                (is (= [{:type :variable, :name "a"} {:type :and} {:type :variable, :name "f(x)"}] (lex "a && f(x)")))
                (is (= [{:type :variable, :name "f(x)"} {:type :and} {:type :variable, :name "f(y)"}] (lex "f(x) && f(y)")))
                )))
