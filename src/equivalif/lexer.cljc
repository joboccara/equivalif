(ns equivalif.lexer
  (:require [clojure.string :as string]
            [equivalif.string-helpers :as string-helpers]))

(declare adjacent-chars-regex closing-paren-position first-function-call-position function-call? function-tokens-to-string replace-slice slice string-to-token)

(defn extract-tokens
  "Transforms source code into a list of tokens"
  [source]
  (if (empty? source)
    []
    (let [substrings (string/split source #" ")
          symbols (mapcat #(string-helpers/split-keep-separator % adjacent-chars-regex) substrings)]
         (map string-to-token symbols))))

(def adjacent-chars-regex #"\(|\)|!")

(defn string-to-token
  "Transforms a string symbol into a token"
  [string-symbol]
  (cond
    (= string-symbol "&&") {:type :and}
    (= string-symbol "||") {:type :or}
    (= string-symbol "!") {:type :not}
    (= string-symbol "(") {:type :open}
    (= string-symbol ")") {:type :close}
    :else {:type :variable, :name string-symbol}))

(defn token-to-string
  "Inverse of to-token"
  [token]
  (condp = (:type token)
    :and "&&"
    :or "||"
    :not "!"
    :open "("
    :close ")"
    :variable (:name token)))

(defn isolate-function-calls [tokens]
  (let [begin (first-function-call-position tokens)]
    (if (nil? begin) tokens
        (let [end (closing-paren-position tokens begin)
              function-call-name (function-tokens-to-string (slice tokens begin end))]
          (replace-slice tokens begin end (list {:type :variable, :name function-call-name}))))))

(defn first-function-call-position [tokens]
  (let [positions (keep-indexed #(when (function-call? (first %2) (second %2)) %1) (partition 2 1 tokens))]
    (if (nil? positions) nil (first positions))))

(defn function-call? [token1 token2]
  (and (= (:type token1) :variable) (= (:type token2) :open)))

(defn closing-paren-position
  ([tokens position] (closing-paren-position 0 tokens (+ position 1)))
  ([depth [first-token & rest-tokens] position]
   (cond
     (= (:type first-token) :close) position
     :else (closing-paren-position depth rest-tokens (+ position 1)))))

(defn function-tokens-to-string [tokens]
  (let [string-tokens (map token-to-string tokens)]
    (apply str (concat (list (nth string-tokens 0)); function name
                       (list (nth string-tokens 1)); open paren
                       (interpose " " (slice string-tokens 2 (- (count tokens) 1))); params interpersed with space
                       (list (last string-tokens)); close params
                       ))))

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (concat (take begin-included coll)
          new-slice
          (drop end-excluded coll)))

(def lex (comp isolate-function-calls extract-tokens))
