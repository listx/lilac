(ns lilac.popstate-history
  (:require [clojure.string :as str]
            [lilac.focus :refer [activate]]))

(defn handle-popstate []
  (let [current-hash
        (if (str/blank? js/window.location.hash)
          "#top"
          js/window.location.hash)
        id (.substring current-hash 1)]
    (when-let [target-el (js/document.getElementById id)]
      (activate target-el))))

(defn init []
  (.addEventListener js/window "popstate" handle-popstate))
