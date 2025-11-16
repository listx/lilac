(ns lilac.core
  (:require [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]
            [lilac.lazy-scroll :as lazy-scroll]
            [lilac.toggle-block-metadata :as tbm]))

(defn msg-successful-load []
  "Lilac loaded successfully.")

(history/init)
(tae/init)
(lazy-scroll/init)
(tbm/init)
(js/console.log (msg-successful-load))
