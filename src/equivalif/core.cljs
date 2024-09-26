(ns equivalif.core
  (:require [reagent.dom :as rdom] [equivalif.app]))

(defn app [] equivalif.app/app)

(defn init []
  (rdom/render [app] (.getElementById js/document "app")))
