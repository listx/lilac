(ns lilac.hamburger)

(defn init-sidebar-for-mobile []
  (let [sidebar (.getElementById js/document "tocs")
        hamburger  (.getElementById js/document "hamburger")]

    ;
    (.addEventListener hamburger "click"
      (fn [e]
        (.stopPropagation e)
        (-> sidebar .-classList (.toggle "active"))))

    ;
    (.addEventListener js/document "click"
      (fn [e]
        (when (and (-> sidebar .-classList (.contains "active"))
                   (not (.contains sidebar (.-target e))))
          (-> sidebar .-classList (.remove "active")))))))

(defn init []
  (.addEventListener js/document "DOMContentLoaded"
    (fn [_] (init-sidebar-for-mobile))))
