(ns lilac.core
  (:require [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]
            [lilac.hamburger :as hamburger]
            [lilac.lazy-scroll :as lazy-scroll]
            [lilac.code-block-controls :as cbc]))

(history/init)
(tae/init)
(lazy-scroll/init)
(cbc/init)
(hamburger/init)
(js/console.log "Lilac loaded successfully.")
