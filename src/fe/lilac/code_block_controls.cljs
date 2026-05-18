(ns lilac.code-block-controls)

(defn- init-toggle-code-block-body []
  (let [body-buttons ;
          (.querySelectorAll js/document "button.toggle-code-block-body")]
      (doseq [button body-buttons]
        (.addEventListener button "click"
          (fn [event]
            (when-let [grandparent
                         (some-> button (.-parentElement) (.-parentElement))]
              (let [body (.querySelector grandparent ".code-block-body")
                    is-anonymous
                      (not (.querySelector grandparent ".code-block-meta"))
                    border "bottom-rounded"
                    controls-inline
                      (.querySelector
                       grandparent
                       ".code-block-controls-inline")]
                ;
                (.toggle (.-classList body) "collapsed")
                (.toggle (.-classList button) "action-collapse")
                (.toggle (.-classList controls-inline) "hide-border-bottom")
                (.toggle (.-classList controls-inline) border))))))))

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
  (init-toggle-buttons)
  (init-toggle-code-block-body))
