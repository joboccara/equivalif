(ns equivalif.ast-builder-test
 (:require [clojure.test :refer :all]
           [equivalif.ast-builder :refer :all]))

(deftest and-expression
  (testing "Returns a list representing the boolean expression"
    (is (= '(and (clojure.core/identity a) (clojure.core/identity b)) (parse "a && b")))))

(deftest or-expression
  (testing
    (is (= '(or (clojure.core/identity a) (clojure.core/identity b)) (parse "a || b")))))

(deftest not-expression
  (testing
   (is (= '(not (clojure.core/identity a)) (parse "!a")))))

(deftest single-symbol
  (testing
   (is (= '(clojure.core/identity a) (parse "a")))))

(deftest redundant-external-parens
  (testing
   (is (= '(clojure.core/identity a) (parse "(a)")))))

(deftest redundant-internal-parens
  (testing (is (= '(and (clojure.core/identity a) (and (clojure.core/identity b) (clojure.core/identity c)))
                  (parse "a && ((b && c))")))))

(deftest nested-expressions
  (testing
   (is (= '(and
            (or
             (clojure.core/identity a)
             (not
              (and
               (clojure.core/identity b)
               (clojure.core/identity c))))
            (or
             (clojure.core/identity a)
             (and
              (clojure.core/identity c)
              (clojure.core/identity d))))
          (parse "(a || (!(b && c))) && (a || (c && d))")))))

(deftest precedence-not-over-and
  (testing (is (= '(and
                    (clojure.core/identity a)
                    (not (clojure.core/identity b)))
                  (parse "a && !b")))))
                  
(deftest precedence-not-over-or
  (testing (is (= '(or
                    (clojure.core/identity a)
                    (not (clojure.core/identity b)))
                  (parse "a || !b")))))

(deftest precedence-and-over-or
  (testing (is (= '(or
                    (and (clojure.core/identity a) (clojure.core/identity b))
                    (clojure.core/identity c))
                  (parse "a && b || c")))))

(deftest precedence-two-ands-over-or
  (testing (is (= '(or
                    (and (clojure.core/identity a) (clojure.core/identity b))
                    (and (clojure.core/identity c) (clojure.core/identity d)))
                  (parse "a && b || c && d")))))

(deftest consecutive-nots
  (testing (is (= '(not (not (clojure.core/identity a)))
                  (parse "!!a")))))

(deftest precedence-in-nested-expressions
  (testing (is (= '(and
                    (clojure.core/identity a)
                    (and
                     (clojure.core/identity b)
                     (not (clojure.core/identity c))))
                  (parse "a && (b && !c)")))))

(deftest consecutive-ands
  (testing (is (= '(and (clojure.core/identity a) (and (clojure.core/identity b) (clojure.core/identity c)))
                  (parse "a && b && c")))))

(deftest consecutive-ors
  (testing (is (= '(or (clojure.core/identity a) (or (clojure.core/identity b) (clojure.core/identity c)))
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