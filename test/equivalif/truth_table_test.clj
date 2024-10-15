(ns equivalif.truth-table-test
  (:require [clojure.test :refer [deftest is testing]]
            [equivalif.truth-table :refer [find-vars truth-table]]))

(deftest find-vars-test
  (testing "Returns all the variables in a boolean expression"
    (is (= '(a b c) (find-vars "(a && b) || ((!b) || c)")))))

(deftest variable-in-if-else-block
  (testing (is (= '(a b) (find-vars "if (a && b) {a} else {b}")))))

(deftest code-blocks-are-not-variables
  (testing "A variable that is only present as the body of an if or else branch is a code block and not a variable"
    (is (= '(a b) (find-vars "if (a && b) {codeblock} else {b}")))))

(deftest variable-in-if-condition
  (testing "A variable in the condition of an if is a variable"
    (is (= '(a) (find-vars "if (a) {codeblock1} {codeblock2}")))))

(deftest truth-table-test
  (testing "Returns the value of a boolean expression for each combination of values of its variables"
    (is (= [{:variables {'a false, 'b false}, :result false},
            {:variables {'a false, 'b true}, :result false},
            {:variables {'a true, 'b false}, :result false},
            {:variables {'a true, 'b true}, :result true}]
           (truth-table "a && b")))))

(deftest truth-table-single-expression
  (testing
    (is (= [{:variables {'a false}, :result false},
            {:variables {'a true}, :result true}]
           (truth-table "a")))))

(deftest truth-table-if-else
    (testing (is (= [{:variables {'a false, 'b false}, :result false},
                     {:variables {'a false, 'b true}, :result true},
                     {:variables {'a true, 'b false}, :result false},
                     {:variables {'a true, 'b true}, :result true}]
                    (truth-table "if (a && b) {a} else {b}")))))

(deftest nested-ifs
  (testing (is (= [{:variables {'a false, 'b false}, :result false}
                   {:variables {'a false, 'b true}, :result "c"}
                   {:variables {'a true, 'b false}, :result "c"}
                   {:variables {'a true, 'b true}, :result true}]
                  (truth-table "if (a) {if (b) {a} else {c}} else {if (!b) {a} else {c}}")))))