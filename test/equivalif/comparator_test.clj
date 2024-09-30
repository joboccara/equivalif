(ns equivalif.comparator-test
  (:require [clojure.test :refer :all]
            [equivalif.comparator :refer :all]))

(deftest comparable-expressions
  (testing "Returns a boolean indicating if the expressions are comparable and can be invoked with truth-table-diff"
    (is (comparable? "a && b" "a || b"))))

(deftest non-comparable-expressions
  (testing "Returns a boolean indicating if the expressions are comparable and can be invoked with truth-table-diff"
    (is (not (comparable? "a && b" "a || c")))))

(deftest equivalent-expressions
  (testing "The diff between two equivalent expressions is empty"
    (is (= [] (truth-table-diff
               "a && (b || c)"
               "(a && b) || (a && c)")))))

(deftest non-equivalent-expressions
  (testing "The diff between two non equivalent expressions is the variable combinations
            where the results are different along with the results"
    (is (= [{:variables {'a false, 'b true} :first false :second true}
            {:variables {'a true, 'b false} :first false :second true}]
           (truth-table-diff
            "a && b"
            "a || b")))))

(deftest comparing-non-comparable-expressions
  (testing
  (let [diff `(truth-table-diff "a && b" "a && c")]
   (is (thrown? clojure.lang.ExceptionInfo (eval diff)))
    (try (eval diff) (catch clojure.lang.ExceptionInfo e (is (= "Truth tables are not comparable" (ex-message e))))))))

(deftest compare-variable-maps-first-less
  (testing "Lexicographical order (based on alphabetical order of keys) of values of 2 maps with identical keys"
    (is (= -1 (compare-variable-maps {'a 1 'b 2} {'a 1 'b 3})))))

(deftest compare-variable-maps-second-less
  (testing
    (is (= 1 (compare-variable-maps {'a 4 'b 2} {'a 1 'b 3})))))

(deftest compare-variable-maps-same
  (testing
    (is (= 0 (compare-variable-maps {'a 1 'b 2} {'a 1 'b 2})))))

(deftest compare-variable-maps-first-less-shuffled-keys
  (testing
    (is (= -1 (compare-variable-maps {'b 2 'a 1} {'a 1 'b 3})))))

(deftest compare-variable-maps-second-less-shuffled-keys
  (testing
    (is (= 1 (compare-variable-maps {'b 2 'a 4} {'a 1 'b 3})))))

(deftest compare-variable-maps-same-shuffled-keys
  (testing
    (is (= 0 (compare-variable-maps {'b 2 'a 1} {'a 1 'b 2})))))