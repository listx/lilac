(ns lilac.core
  (:require [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]
            [lilac.hamburger :as hamburger]
            [lilac.lazy-scroll :as lazy-scroll]
            [lilac.block-controls :as bc]))

(history/init)
(tae/init)
(lazy-scroll/init)
(bc/init)
(hamburger/init)
(js/console.log "Lilac loaded successfully.")
