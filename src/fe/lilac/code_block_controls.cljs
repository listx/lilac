(ns lilac.code-block-controls)

(defn- init-toggle-buttons []
  (let [buttons (-> js/document
                    (.querySelectorAll "button.toggle-code-block-meta-raw"))]
    (doseq [button buttons]
      (.addEventListener button "click"
        (fn [event]
          (when-let [grandparent
                       (some-> button
                               (.-parentElement)
                               (.-parentElement))]
              (let [raw (.querySelector grandparent ".code-block-meta-raw")
                    minimal-meta (.querySelector grandparent ".code-block-meta")
                    body (.querySelector grandparent ".code-block-body")]
                (.toggle (.-classList raw) "hidden")
                (when (or (.contains (.-classList body) "fully-rounded")
                          (nil? minimal-meta))
                  (.toggle (.-classList body) "fully-rounded")))))))))

(defn init []
  (init-toggle-buttons))
