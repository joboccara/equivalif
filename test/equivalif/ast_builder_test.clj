(ns equivalif.ast-builder-test
 (:require [clojure.test :refer [deftest is testing]]
           [equivalif.ast-builder :refer [parse]]))

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

(deftest redundant-external-parens
  (testing
   (is (= 'a (parse "(a)")))))

(deftest redundant-internal-parens
  (testing (is (= '(and a (and b c))
                  (parse "a && ((b && c))")))))

(deftest nested-expressions
  (testing
   (is (= '(and (or a (not (and b c))) (or a (and c d)))
          (parse "(a || (!(b && c))) && (a || (c && d))")))))

(deftest precedence-not-over-and
  (testing (is (= '(and a (not b))
                  (parse "a && !b")))))
                  
(deftest precedence-not-over-or
  (testing (is (= '(or a (not b))
                  (parse "a || !b")))))

(deftest precedence-and-over-or
  (testing (is (= '(or (and a b) c)
                  (parse "a && b || c")))))

(deftest precedence-two-ands-over-or
  (testing (is (= '(or (and a b) (and c d))
                  (parse "a && b || c && d")))))

(deftest consecutive-nots
  (testing (is (= '(not (not a))
                  (parse "!!a")))))

(deftest precedence-in-nested-expressions
  (testing (is (= '(and a (and b (not c)))
                  (parse "a && (b && !c)")))))

(deftest consecutive-ands
  (testing (is (= '(and a (and b c))
                  (parse "a && b && c")))))

(deftest consecutive-ors
  (testing (is (= '(or a (or b c))
                  (parse "a || b || c")))))

(deftest close-without-open
  (testing (is (= '() (parse ")")))))

(deftest unbalanced-parens-test1
  (testing (is (= '() (parse "a && (b || (c)")))))

(deftest unbalanced-parens-test2
  (testing (is (= '() (parse "a && (b && c))")))))

(deftest single-not
  (testing (is (= '() (parse "!")))))

(deftest missing-first-arg-in-and
  (testing (is (= '() (parse "&& a")))))

(deftest missing-second-arg-in-and
  (testing (is (= '() (parse "a &&")))))

(deftest extra-and-paramter
  (testing (is (= '() (parse "a && b c")))))

(deftest empty-expression
  (testing (is (= '() (parse "")))))

(deftest and-custom-expression
  (testing (is (= (list 'and 'a (symbol "value > 0"))
                  (parse "a && (value > 0)")))))

(deftest custom-expressions-must-at-inner-level
  (testing (and (is (= '() (parse "value > 0")))
                (is (= '() (parse "a && value > 0"))))))

(deftest if-else
  (testing (and (is (= '(if condition a else b) (parse "if condition a else b")))
                (is (= '(if (and a b) a else b) (parse "if (a && b) a else b")))
                (is (= (list 'if '(and a b) (symbol "do stuff") 'else 'b) (parse "if (a && b) {do stuff} else {b}")))
                (is (= (list 'if '(and a b) 'a 'else (symbol "do stuff")) (parse "if (a && b) a else {do stuff}")))
                (is (= (list 'if '(and a b) (symbol "do stuff") 'else (symbol "do other stuff")) (parse "if (a && b) {do stuff} else {do other stuff}")))
                )))

(deftest multiline-if-else
  (testing (is (= (list 'if 'a (symbol "do stuff\nand more") 'else (symbol "do other stuff\nand still more"))
                  (parse "if a {
do stuff
and more
} else
{
do other stuff
and still more
}")))))

(deftest else-block-requires-else-keyword
  (testing (is (= '() (parse "if (a) {b} {c}")))))

(deftest else-keyword-requires-else-block
  (testing (is (= '() (parse "if (a) {b} else")))))
