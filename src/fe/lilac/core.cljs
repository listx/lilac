(ns lilac.core
  (:require [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]))

(defn msg-successful-load []
  "Lilac loaded successfully.")

(history/init)
(tae/init)
(js/console.log (msg-successful-load))
