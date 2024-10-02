(ns equivalif.app
  (:require [reagent.core :as r]
            [equivalif.evaluator :as evaluator]
            [equivalif.comparator :as comparator]))

(declare expressions-form non-comparable-expressions text-input truth-table)

(def app 
  (let [expressions (r/atom {:expression1 "a && b", :expression2 "a || b"})]
  (fn []
   [:<>
     [:div
       [:h1 "Equivalif"]
       [:p "You'll never wonder if two ifs are equivalent again"]]
     (conj
       (expressions-form expressions)
       (if (comparator/comparable? (:expression1 @expressions) (:expression2 @expressions))
         (truth-table expressions)
         non-comparable-expressions))])))

(defn expressions-form [expressions]
     [:form
       [:div
         [:label {:for "expression1"} "Expression 1"] (text-input expressions :expression1)]
       [:div
         [:label {:for "expression2"} "Expression 2"] (text-input expressions :expression2)]])

(defn truth-table [expressions]
  (let [variables (evaluator/find-vars (:expression1 @expressions))
        compared-truth-table (comparator/compared-truth-table (:expression1 @expressions) (:expression2 @expressions))]
  [:div
   [:table
    [:thead
     [:tr
      (for [variable variables]
        ^{:key (str "variable-" (name variable))} [:th  variable])
      ^{:key (str "expression-1")} [:th "Expression 1"]
      ^{:key (str "expression-2")} [:th "Expression 2"]]]
    [:tbody
     (for [line compared-truth-table]
     ^{:key (str (:variables line))}
      [:tr
       (for [variable variables]
         ^{:key (str "value-" (name variable))} [:th (str (get (:variables line) variable))])
       ^{:key "result-1"} [:th (str (:first line))]
       ^{:key "result-2"} [:th (str (:second line))]])]]]))

(def non-comparable-expressions
  [:div "The expressions don't contain the same variables"])

(defn text-input [expressions kw]
  [:input {:type "text"
            :id (name kw)
            :name (name kw)
            :on-change #(swap! expressions assoc kw (-> % .-target .-value))}])
