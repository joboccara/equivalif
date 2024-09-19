(ns equivalif.lexer-test
 (:require [clojure.test :refer :all]
            [equivalif.lexer :refer :all]))

(deftest empty-source
  (testing "Empty source returns empty array"
   (is (= [] (lex "")))))