(ns lilac.lazy-scroll
  (:require [lilac.focus :refer [activate]]
            [clojure.string :as str]))

(defn- href->id [href]
  (let [href (.-hash (.-location js/window))
        href-final (if str/blank? href "#top")
        id (.substring href 1)]
    id))

(defn- get-breadcrumbs-height []
  (or (some-> js/document
              (.querySelector ".breadcrumbs")
              (.-parentElement)
              (.-offsetHeight))
      0))

(defn- visible? [elt]
  (let [rect (.getBoundingClientRect elt) ;
        bch (get-breadcrumbs-height)
        rect-top (- (.-top rect) bch)
        rect-bottom (- (.-bottom rect) bch)
        window-height (.-innerHeight js/window)
        is-visible (and (< rect-top window-height) ;
                        (> rect-bottom 0))]
    is-visible))

(defn- maybe-scroll [elt]
  (when-not (visible? elt)
    (.scrollIntoView elt)))

(defn- maybe-scroll-by-hash [_popstate-event]
  (let [href (.-hash (.-location js/window))
        id (href->id href)]
    (when-let [target-element (-> js/document (.getElementById id))]
      (activate target-element) ;
      (maybe-scroll target-element)))) ;

(defn- weaken-anchor-links []
  (let [links (-> js/document (.querySelectorAll "a[href^=\"#\"]"))]
    (doseq [link links]
      (.addEventListener link "click"
        (fn [event]
          (let [href (.getAttribute link "href")
                id (href->id href)]
            (when-let [target-element (-> js/document (.getElementById id))]
              (do
                (.preventDefault event) ;
                (.pushState (.-history js/window) nil nil href) ;
                (maybe-scroll-by-hash nil)))))))))

(defn init [] ;
  (weaken-anchor-links) ;
  (.addEventListener js/window "popstate" maybe-scroll-by-hash) ;
  (maybe-scroll-by-hash nil)) ;
