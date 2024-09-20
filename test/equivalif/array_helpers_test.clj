(ns equivalif.array-helpers-test
 (:require [clojure.test :refer :all]
            [equivalif.array-helpers :refer :all]))

(deftest alternate-empty-empty
  (testing "Alternating two empty array returns an empty array"
    (is (= [] (alternate [] [])))))

(deftest alternate-full-empty
  (testing "Alternating a full array with an empty array returns the full array"
    (is (= [1 2 3] (alternate [1 2 3] [])))))

(deftest alternate-empty-full
  (testing "Alternating an empty array with a full array returns the full array"
    (is (= [1 2 3] (alternate [] [1 2 3])))))

(deftest alternate-small-large
  (testing (is (= [1 "a" "b"] (alternate [1] ["a" "b"])))))

(deftest alternate-large-small
  (testing (is (= [1 "a" 2] (alternate [1 2] ["a"])))))

(deftest alternate-same-same
  (testing (is (= [1 "a" 2 "b"] (alternate [1 2] ["a" "b"])))))