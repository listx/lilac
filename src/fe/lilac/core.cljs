(ns lilac.core
  (:require [lilac.autofocus :as af]
            [lilac.popstate-history :as history]
            [lilac.toc-active-entries :as tae]
            [lilac.lazy-scroll :as lazy-scroll]
            [lilac.toggle-block-metadata :as tbm]))

(af/init)
(history/init)
(tae/init)
(lazy-scroll/init)
(tbm/init)
(js/console.log "Lilac loaded successfully.")
