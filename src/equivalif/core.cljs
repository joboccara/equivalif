(ns equivalif.core
  (:require [equivalif.app] [reagent.dom.client :as rdom-client]))

(defn app [] equivalif.app/app)

(defn init []
  (rdom-client/render (rdom-client/create-root (.getElementById js/document "app")) [app]))
