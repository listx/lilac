(ns lilac.autofocus)

(defn focus-main []
  (some-> (.getElementById js/document "main-content-column")
          (.focus)))

(defn init []
  (.addEventListener js/document "DOMContentLoaded" focus-main))
