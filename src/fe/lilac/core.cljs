(ns lilac.core
  (:require [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]
            [lilac.lazy-scroll :as lazy-scroll]))

(defn msg-successful-load []
  "Lilac loaded successfully.")

(history/init)
(tae/init)
(lazy-scroll/init)
(js/console.log (msg-successful-load))
