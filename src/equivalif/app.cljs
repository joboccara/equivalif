(ns equivalif.app
  (:require [reagent.core :as r]))

(defn on-submit [event form-data]
  (.preventDefault event))

(declare text-input expressions-form)

(def app 
  (let [expressions (r/atom {:expression1 "", :expression2 ""})]
  (fn []
    (expressions-form expressions))))

(defn expressions-form [expressions]
   [:<>
     [:div
       [:h1 "Equivalif"]
       [:p "You'll never wonder if two ifs are equivalent again"]] 
     [:form
       {:on-submit (fn [event] (on-submit event expressions))}
       [:div
         [:label {:for "expression1"} "Expression 1"] (text-input expressions :expression1)]
       [:div
         [:label {:for "expression2"} "Expression 2"] (text-input expressions :expression2)]
       [:div
         [:button {:type "submit"} "Compare"]]]])

(defn text-input [expressions kw]
  [:input {:type "text"
            :id (name kw)
            :name (name kw)
            :on-change #(swap! expressions assoc kw (-> % .-target .-value))}])
