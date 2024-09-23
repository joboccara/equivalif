(ns equivalif.lexer
  (:require [clojure.string :as string]
            [equivalif.string-helpers :as string-helpers]))

(declare to-token paren-regex)

(defn lex
  "Transforms source code into a list of tokens"
  [source]
  (if (empty? source)
    []
    (let [substrings (string/split source #" ")
          symbols (mapcat #(string-helpers/split-keep-separator % paren-regex) substrings)]
         (map to-token symbols))))

(def paren-regex #"\(|\)")

(defn to-token
  "Transforms a string symbol into a token"
  [symbol]
  (cond
    (= symbol "&&") {:type :and}
    (= symbol "||") {:type :or}
    (= symbol "(") {:type :open}
    (= symbol ")") {:type :close}
    :else {:type :variable, :name symbol}))
