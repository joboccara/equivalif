(ns equivalif.app
  (:require [reagent.core :as r]
            [equivalif.ast-builder :as ast]
            [equivalif.truth-table :as truth-table]
            [equivalif.comparator :as comparator]))

(declare expressions-form expression-input invalid-expression? matching-class non-comparable-expressions text-input th-align-style truth-table)

(def default-expression1
  "if (tomIsNice && !(tomIsNice && jerryIsMean)) {
  patTomAndJerry();
} else {
  letTheDogIn();
}")

(def default-expression2
 "if (tomIsNice && !jerryIsMean) {
  patTomAndJerry();
} else if (jerryIsMean) {
  letTheDogIn();
}")

(def app 
  (let [expressions (r/atom {:expression1 default-expression1, :expression2 default-expression2})]
  (fn []
   [:<>
     [:div
       [:img {:src "/logo.png" :class "logo"}]
       [:p {:class "subtitle"} "You'll never wonder if two ifs are equivalent again"]]
     (conj
       (expressions-form expressions)
       (let [expression1 (:expression1 @expressions) expression2 (:expression2 @expressions)]
       (cond
         (or (empty? expression1) (empty? expression2)) nil
         (or (invalid-expression? expression1) (invalid-expression? expression2)) nil
         (comparator/comparable? expression1 expression2) (truth-table expressions)
         :else non-comparable-expressions)))])))

(defn expressions-form [expressions]
     [:form
       (expression-input expressions :expression1 "Compare")
       (expression-input expressions :expression2 "With")])

(defn expression-input [expressions kw display]
  [:div {:class "expression-input-with-label"}
         [:label {:for (name kw) :class "expression-label"} display]
         (text-input expressions kw)
         (when (invalid-expression? (kw @expressions)) [:div {:class "invalid-expression"} "Invalid expression"])])

(defn truth-table [expressions]
  (let [variables (truth-table/find-vars (:expression1 @expressions))
        compared-truth-table (comparator/compared-truth-table (:expression1 @expressions) (:expression2 @expressions))]
  [:div
   [:table {:class "truth-table"}
    [:tbody
     [:tr
     (for [variable variables]
       ^{:key (str "variable-" (name variable))} [:th  variable])
     ^{:key (str "expression-1")} [:th {:style (th-align-style (:expression1 @expressions))} (:expression1 @expressions)]
     ^{:key (str "expression-2")} [:th {:style (th-align-style (:expression2 @expressions))} (:expression2 @expressions)]]
     (for [line compared-truth-table]
     ^{:key (str (:variables line))}
     [:tr
       (for [variable variables]
         ^{:key (str "value-" (name variable))} [:td (str (get (:variables line) variable))])
       ^{:key "result-1"} [:td {:class (matching-class (:first line) (:second line))} (str (:first line))]
       ^{:key "result-2"} [:td {:class (matching-class (:first line) (:second line))} (str (:second line))]])]]]))

(defn matching-class [v1 v2]
  (if (= v1 v2) "match" "mismatch"))

(defn th-align-style [expression]
  (if (some #{\newline} expression) {:text-align :left} {:text-align :center}))

(defn invalid-expression? [expression]
  (= (ast/parse expression) '()))

(def non-comparable-expressions
  [:div {:class "no-truth-table"} "The expressions don't contain the same variables"])

(defn text-input [expressions kw]
  [:textarea {:class "expression-input"
              :type "text"
              :id (name kw)
              :name (name kw)
              :value (kw @expressions)
              :on-change #(swap! expressions assoc kw (-> % .-target .-value))}])
