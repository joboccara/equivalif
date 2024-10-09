(ns equivalif.string-helpers-test
 (:require [clojure.test :refer [deftest is testing]]
            [equivalif.string-helpers :refer [split-keep-separator]]))

(deftest regex-inside
  (testing "Returns the separators and the substrings between the separators in their initial order"
    (is (= ["abc" "4" "ef"] (split-keep-separator "abc4ef" #"\d")))))

(deftest regex-first
  (testing (is (= ["4" "abc"] (split-keep-separator "4abc" #"\d")))))

(deftest regex-last
  (testing (is (= ["abc" "4"] (split-keep-separator "abc4" #"\d")))))

(deftest regex-surrounding
  (testing (is (= ["3" "abc" "4"] (split-keep-separator "3abc4" #"\d")))))

(deftest regex-absent
  (testing (is (= ["abc"] (split-keep-separator "abc" #"\d")))))

(deftest regex-empty
  (testing (is (= [] (split-keep-separator "" #"\d")))))

(deftest consectutive-occurences-of-regex
  (testing (is (= ["a" "a" "b" "a"] (split-keep-separator "aaba" #"a")))))