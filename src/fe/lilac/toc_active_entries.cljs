(ns lilac.toc-active-entries
  (:require [clojure.walk :refer [keywordize-keys]]
            [clojure.set]))

(defonce visible-headings (atom #{}))

(defn get-heading-ancestry []
  (if-let [script-tag (.querySelector js/document "#heading-ancestry")]
    (let [json-string (.-textContent script-tag)
          js-object (.parse js/JSON json-string)]
      (keywordize-keys (js->clj js-object)))
    (do
      (.error js/console "Warning: Heading ancestry data not found; using empty map.")
      {})))

(defn update-visible-headings [entries]
  (doseq [entry entries]
    (let [heading-container-id (.. entry -target -id)
          heading-id (subs heading-container-id 18)
          visible? (.-isIntersecting entry)
          modifier (if visible? conj disj)]
      (swap! visible-headings modifier heading-id))))

(defn update-toc-highlights [ancestry visible-headings]
  (let [toc-entries (.querySelectorAll js/document ".toc-entry")
        toc-container (.querySelector js/document ".toc")
        visible-headings-with-ancestors
          (reduce (fn [acc id]
                      (let [ancestors (set (get ancestry (keyword id) []))]
                        (clojure.set/union acc ancestors)))
            visible-headings
            visible-headings)]

    (doseq [toc-entry toc-entries]
      (when-let [toc-entry-link (.querySelector toc-entry "a")]
        (let [heading (subs (.-hash toc-entry-link) 1)
              class-list (.-classList toc-entry)]
          (cond
            (contains? visible-headings heading)
            (do
              (.add class-list "active")
              (.remove class-list "active-ancestor"))

            (contains? visible-headings-with-ancestors heading)
            (do
              (.add class-list "active-ancestor")
              (.remove class-list "active"))

            :else
            (do
              (.remove class-list "active")
              (.remove class-list "active-ancestor"))))))))

(defn maybe-scroll-toc [visible-headings]
  (let [toc-entries (.querySelectorAll js/document ".toc-entry")
        toc-container (.querySelector js/document ".toc")
        all-active-entries
          (filter (fn [toc-entry]
                    (when-let [toc-entry-link (.querySelector toc-entry "a")]
                      (contains? visible-headings (subs (.-hash toc-entry-link) 1))))
                  toc-entries)

        first-active (first all-active-entries)
        last-active (last all-active-entries)]

    (when (seq all-active-entries)
      (let [toc-rect (.getBoundingClientRect toc-container)
            first-active-rect (.getBoundingClientRect first-active)
            last-active-rect (.getBoundingClientRect last-active)]

        (cond
          (< (.-top first-active-rect) (.-top toc-rect))
          (if (= first-active (first toc-entries))
            (.scrollTo toc-container #js
                       {:top 0 :behavior "instant"})
            (.scrollIntoView first-active #js
                             {:behavior "instant" :block "start"}))

          (> (.-bottom last-active-rect) (.-bottom toc-rect))
          (if (= last-active (last toc-entries))
            (.scrollTo toc-container #js
                       {:top (.-scrollHeight toc-container) :behavior "instant"})
            (.scrollIntoView last-active #js
                             {:behavior "instant" :block "end"}))

          :else nil)))))

(defn init []
  (let [heading-ancestry (get-heading-ancestry)]

    (add-watch visible-headings :lilac-toc-updater
      (fn [_key atom _old-state _new-state]
        (update-toc-highlights heading-ancestry @atom)
        (maybe-scroll-toc @atom)))

    (let [options #js {:rootMargin "0px 0px 0px 0px" :threshold 0}
          heading-containers (.querySelectorAll js/document ".heading-container")
          observer (new js/IntersectionObserver update-visible-headings options)]
      (doseq [heading-container heading-containers]
        (.observe observer heading-container)))

    (let [toc-entry-links (.querySelectorAll js/document ".toc-entry a")]
      (doseq [link toc-entry-links]
        (.addEventListener link "click" #(reset! visible-headings #{}))))))
