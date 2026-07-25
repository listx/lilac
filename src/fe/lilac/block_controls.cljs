(ns lilac.block-controls)

(defonce state
  (atom {:block-ids {} ;
         :updated-block-id nil})) ;

;; Pure data calculation function: returns initial map from HTML markers
(defn- calculate-initial-state [container]
  (let [body (.querySelector container ".block-body")
        is-collapsed? (.contains (.-classList body) "collapsed")
        has-collapse-button? (.contains (.-classList body) "collapsible")
        is-anonymous? (.contains (.-classList body) "anonymous")]
    {:meta-visible?        false
     :body-visible?        (not is-collapsed?)
     :has-collapse-button? has-collapse-button?
     :anonymous?           is-anonymous?}))

(defn- ensure-block-state [st block-id block-el]
  (if (contains? (:block-ids st) block-id)
    st
    (assoc-in st [:block-ids block-id] (calculate-initial-state block-el))))

(defn- handle-click! [e]
  (when-let [btn (.closest (.-target e) "[data-action]")]
    (when-let [block (.closest btn ".block-container")]
      (let [block-id (keyword (.-id block))
            action  (.getAttribute btn "data-action")]

        (case action
          ("toggle-block-meta-other" "toggle-block-body")
          (let [state-key (if (= action "toggle-block-meta-other") ;
                            :meta-visible?
                            :body-visible?)]
            (swap! state #(-> % ;
                              (ensure-block-state block-id block)
                              (update-in [:block-ids block-id state-key] not)
                              (assoc :updated-block-id block-id))))

          (js/console.warn "[WARN] Unrecognized action:" action))))))

(defn- toggle-class! [el class-name enable?]
  (when el
    (.toggle (.-classList el) class-name enable?)))

(def rounding-styles
  #{"fully-rounded" "top-rounded" "bottom-rounded" "not-rounded"})

(defn- set-enum-class! [el all-variants target-variant]
  (when el
    (let [class-list (.-classList el)]
      (doseq [variant all-variants]
        (.remove class-list variant))
      (when target-variant
        (.add class-list target-variant)))))

(defn- render-block! [block-id]
  (if-let [container (.getElementById js/document (name block-id))]
    (let [block-state (get-in @state [:block-ids block-id]) ;
          anon?            (:anonymous? block-state)
          body-vis?        (:body-visible? block-state)
          collapse-button? (:has-collapse-button? block-state)
          meta-vis?        (:meta-visible? block-state)

          other-meta (.querySelector container ".block-meta-other")
          body (.querySelector container ".block-body")
          controls (.querySelector container ".block-controls-inline")

          body-needs-full-rounding?
            (and anon? (not meta-vis?) (not collapse-button?) body-vis?)

          rounding-style ;
            (cond
              (and anon? meta-vis? body-vis?) "not-rounded"
              (and anon? (not meta-vis?) (not body-vis?)) "fully-rounded"
              (and anon? meta-vis? (not body-vis?)) "bottom-rounded"
              (and anon? (not meta-vis?) body-vis?) "top-rounded"
              (and (not anon?) meta-vis? body-vis?) "not-rounded"
              (and (not anon?) (not meta-vis?) (not body-vis?)) "bottom-rounded"
              (and (not anon?) meta-vis? (not body-vis?)) "bottom-rounded"
              (and (not anon?) (not meta-vis?) body-vis?) "not-rounded"
              :else nil)]

      ;
      (toggle-class! other-meta "hidden" (not meta-vis?))
      (toggle-class! body "collapsed" (not body-vis?))
      (toggle-class! body "fully-rounded" body-needs-full-rounding?)
      (toggle-class! controls "hide-border-bottom" body-vis?)
      (set-enum-class! controls rounding-styles rounding-style))

    (js/console.warn "[WARN] Render target not found for ID:"
                     (name block-id))))

(defn init []
  (add-watch state :render
    (fn [_ _ _ new-state]
      (when-let [block-id (:updated-block-id new-state)]
        (render-block! block-id)))) ;
  (.addEventListener js/document "click" handle-click!)) ;
