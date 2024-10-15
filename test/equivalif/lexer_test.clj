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

(deftest if-else-block-tokens
  (testing "if else and blocks return their tokens"
    (is (= [{:type :if} {:type :open-block} {:type :variable, :name "a"} {:type :close-block} {:type :else} {:type :open-block} {:type :variable, :name "b"} {:type :close-block}]
           (lex "if {a} else {b}")))))

(deftest spaces-are-removed-around-block-delimitations
  (testing
   (is (= [{:type :if} {:type :open-block}
           {:type :variable, :name "a"} {:type :variable, :name "\n"} {:type :variable, :name "b"}
           {:type :close-block} {:type :else} {:type :open-block}
           {:type :variable, :name "c"}
           {:type :close-block}]
          (lex "if {\na\nb\n}\nelse\n{c}")))))

(deftest function-calls-converted-to-variables
  (testing (and (is (= [{:type :variable, :name "f(x + 1)"}] (lex "f(x + 1)")))
                (is (= [{:type :variable, :name "a"} {:type :and} {:type :variable, :name "f(x)"}] (lex "a && f(x)")))
                (is (= [{:type :variable, :name "f(x)"} {:type :and} {:type :variable, :name "f(y)"}] (lex "f(x) && f(y)")))
                (is (= [{:type :variable, :name "f(a && b)"}] (lex "f(a && b)")))
                (is (= [{:type :not} {:type :variable, :name "f(a && b)"}] (lex "!f(a && b)")))
                (is (= [{:type :not} {:type :open} {:type :variable, :name "f(a && b)"} {:type :or} {:type :variable, :name "b"} {:type :close}] (lex "!(f(a && b) || b)")))
                (is (= [{:type :variable, :name "f(g ( x ) + h ( y ))"}] (lex "f(g(x) + h(y))")))
                (is (= [] (lex "f(")))
                (is (= [] (lex "f(g(x) + h(y)")))
                (is (= [] (lex "a && f(g(x) + h(y)")))
                )))
