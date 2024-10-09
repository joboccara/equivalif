(ns equivalif.core
  (:require [equivalif.app] [reagent.dom.client :as rdom-client]))

(defn app [] equivalif.app/app)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn init []
  (rdom-client/render (rdom-client/create-root (.getElementById js/document "app")) [app]))
