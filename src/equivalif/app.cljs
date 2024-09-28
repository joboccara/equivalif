(ns equivalif.app
  (:require [reagent.core :as r]))

(defn on-submit [event form-data]
  (.preventDefault event))

(declare text-input)

(def app 
  (let [form-data (r/atom {:expression1 "", :expression2 ""})]
  (fn []
  [:<>
   [:div
    [:h1 "Equivalif"]
    [:p "You'll never wonder if two ifs are equivalent again"]]
   [:form
    {:on-submit (fn [event] (on-submit event form-data))}
    [:div
     [:label {:for "expression1"} "Expression 1"] (text-input form-data :expression1)]
    [:div
     [:label {:for "expression2"} "Expression 2"] (text-input form-data :expression2)]
    [:div
     [:button {:type "submit"} "Compare"]]]])))

(defn text-input [form-data kw]
  [:input {:type "text"
            :id (name kw)
            :name (name kw)
            :on-change #(swap! form-data assoc kw (-> % .-target .-value))}])
