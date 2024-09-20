(ns equivalif.string-helpers-test
 (:require [clojure.test :refer :all]
            [equivalif.string-helpers :refer :all]))

(deftest regex-inside
  (testing "Returns the separators and the substrings between the separators in their initial order"
    (is (= ["abc" "4" "ef"] (split-keep-separator "abc4ef" #"\d")))))

(deftest regex-first
  (testing (is (= ["4" "abc"] (split-keep-separator "4abc" #"\d")))))

(deftest regex-last
  (testing (is (= ["abc" "4"] (split-keep-separator "abc4" #"\d")))))

(deftest regex-surrounding
  (testing (is (= ["3" "abc" "4"] (split-keep-separator "3abc4" #"\d")))))