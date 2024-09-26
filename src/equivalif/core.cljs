(ns equivalif.core
  (:require [reagent.dom :as rdom]))

;; Define a Reagent component
(defn app []
  [:div
   [:h1 "Hello, Equivalif"]
   [:p "You'll never wonder if two ifs are equivalent again"]])

;; Initialize the app and render it into the #app div
(defn init []
  (rdom/render [app] (.getElementById js/document "app")))
