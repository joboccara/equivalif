(ns equivalif.comparator-test
  (:require [clojure.test :refer :all]
            [equivalif.comparator :refer :all]
            [equivalif.evaluator :refer :all]))

(deftest equivalent-expressions
  (testing "The diff between two equivalent expressions is empty"
    (is (= [] (truth-table-diff
               (truth-table "a && (b || c)")
               (truth-table "(a && b) || (a && c)"))))))

(deftest non-equivalent-expressions
  (testing "The diff between two non equivalent expressions is the variable combinations
            where the results are different along with the results"
    (is (= [{:variables {:a false, :b true} :first false :second true}
            {:variables {:a true, :b false} :first false :second true}]
           (truth-table-diff
            (truth-table "a && b")
            (truth-table "a || b"))))))

(deftest compare-variable-maps-first-less
  (testing "Lexicographical order (based on alphabetical order of keys) of values of 2 maps with identical keys"
    (is (= -1 (compare-variable-maps {:a 1 :b 2} {:a 1 :b 3})))))

(deftest compare-variable-maps-second-less
  (testing
    (is (= 1 (compare-variable-maps {:a 4 :b 2} {:a 1 :b 3})))))

(deftest compare-variable-maps-same
  (testing
    (is (= 0 (compare-variable-maps {:a 1 :b 2} {:a 1 :b 2})))))

(deftest compare-variable-maps-first-less-shuffled-keys
  (testing
    (is (= -1 (compare-variable-maps {:b 2 :a 1} {:a 1 :b 3})))))

(deftest compare-variable-maps-second-less-shuffled-keys
  (testing
    (is (= 1 (compare-variable-maps {:b 2 :a 4} {:a 1 :b 3})))))

(deftest compare-variable-maps-same-shuffled-keys
  (testing
    (is (= 0 (compare-variable-maps {:b 2 :a 1} {:a 1 :b 2})))))