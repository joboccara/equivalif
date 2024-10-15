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
   (let [ev #(evaluate '(if condition a b) %)]
     (and (is (= true (ev {'condition true 'a true 'b false})))
          (is (= false (ev {'condition true 'a false 'b true})))
          (is (= false (ev {'condition false 'a true 'b false})))
          (is (= true (ev {'condition false 'a false 'b true})))
          ))))