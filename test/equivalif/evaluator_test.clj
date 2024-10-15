(ns equivalif.evaluator-test
 (:require [clojure.test :refer [deftest is testing]]
           [equivalif.evaluator :refer [evaluate]]))

(deftest evaluate-and
  (testing
   (let [ev #(evaluate '(and a b) %)]
     (and (is (= false (ev {'a false 'b false})))
          (is (= false (ev {'a false 'b true})))
          (is (= false (ev {'a true 'b false})))
          (is (= true (ev {'a true 'b true})))))))

(deftest evaluate-or
  (testing
   (let [ev #(evaluate '(or a b) %)]
     (and (is (= false (ev {'a false 'b false})))
          (is (= true (ev {'a false 'b true})))
          (is (= true (ev {'a true 'b false})))
          (is (= true (ev {'a true 'b true})))))))

(deftest evaluate-not
  (testing
   (let [ev #(evaluate '(not a) %)]
     (and (is (= false (ev {'a true})))
          (is (= true (ev {'a false})))))))

(deftest evaluate-if-else
  (testing
   (let [ev #(evaluate '(if (and a b) a else b) %)]
     (and (is (= false (ev {'a false 'b false})))
          (is (= true (ev {'a false 'b true})))
          (is (= false (ev {'a true 'b false})))
          (is (= true (ev {'a true 'b true})))))))

(deftest evaluate-if-else-with-code-block
  (testing
   (let [ev #(evaluate '(if (and a b) codeblock else b) %)]
     (and (is (= false (ev {'a false 'b false})))
          (is (= true (ev {'a false 'b true})))
          (is (= false (ev {'a true 'b false})))
          (is (= "codeblock" (ev {'a true 'b true})))))))

(deftest evaluate-if-else-with-code-block-with-newline
  (testing
   (let [ev #(evaluate (list 'if '(and a b) (symbol "line1\nline2") 'else 'b) %)]
     (and (is (= false (ev {'a false 'b false})))
          (is (= true (ev {'a false 'b true})))
          (is (= false (ev {'a true 'b false})))
          (is (= "line1\nline2" (ev {'a true 'b true})))))))

(deftest evaluate-if-else-with-two-code-blocks
  (testing
   (let [ev #(evaluate '(if (and a b) codeblock1 else codeblock2) %)]
     (and (is (= "codeblock2" (ev {'a false 'b false})))
          (is (= "codeblock2" (ev {'a false 'b true})))
          (is (= "codeblock2" (ev {'a true 'b false})))
          (is (= "codeblock1" (ev {'a true 'b true})))))))