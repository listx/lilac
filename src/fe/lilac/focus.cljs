(ns lilac.focus)

(defn- deactivate-other-elements [active-id]
  (when-let [main-elt (-> js/document (.querySelector "main"))]
    (let [child-anchors (-> main-elt (.querySelectorAll "[id]"))] ; (ref:child-anchors)
      (doseq [elt child-anchors]
        (when (not= (.-id elt) active-id)
          (let [class-list (.-classList elt)]
            (.remove class-list "active")))))))

(defn- activate-element [elt]
  (let [class-list (.-classList elt)]
    (.add class-list "active")))

(defn activate [elt]
  (deactivate-other-elements (.-id elt))
  (activate-element elt))
