(ns lilac.core
  (:require [lilac.popstate-history :as history]))

(defn msg-successful-load []
  "Lilac loaded successfully.")

(.addEventListener js/document "DOMContentLoaded" history/init)
(js/console.log (msg-successful-load))
