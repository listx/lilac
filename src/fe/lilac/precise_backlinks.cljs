(ns lilac.precise-backlinks)

(defn- ensure-popup! []
  (or (js/document.getElementById "backlinks-popup")
      (let [popup (js/document.createElement "div")]
        (set! (.-id popup) "backlinks-popup")
        (.add (.-classList popup) "popup-backlinks")
        popup)))

(defn- extract-trigger [target selector]
  (when target
    (let [element (if (= (.-nodeType target) js/Node.TEXT_NODE)
                    (.-parentElement target)
                    target)]
      (when element
        (.closest element selector)))))

(defn- parse-backlinks [trigger]
  (let [raw-data (.getAttribute trigger "data-backlinks")] ;
    (js/JSON.parse (or raw-data "[]"))))

(defn- build-backlinks-list [triples]
  (let [ul (js/document.createElement "ul")]
    (if (pos? (.-length triples))
      (doseq [item (array-seq triples)]
        (let [li (js/document.createElement "li")]
          (set! (.-textContent li) (js/JSON.stringify item))
          (.appendChild ul li)))
      (set! (.-textContent ul) "No backlinks found."))
    ul))

(defn- position-popup! [popup x y]
  (let [padding  10
        vw       js/window.innerWidth
        vh       js/window.innerHeight
        pw       (.-offsetWidth popup)
        ph       (.-offsetHeight popup)

        ;; Initial default positioning (+10px offset from cursor)
        target-x (+ x padding)
        target-y (+ y padding)

        ;; Clamp to right edge if popup would overflow viewport on the right.
        final-x  (if (> (+ target-x pw) vw)
                   (max 0 (- vw pw padding))
                   target-x)

        ;; Clamp to bottom edge if popup would overflow viewport on the bottom.
        final-y  (if (> (+ target-y ph) vh)
                   (max 0 (- vh ph padding))
                   target-y)]
    (set! (-> popup .-style .-left) (str final-x "px"))
    (set! (-> popup .-style .-top) (str final-y "px"))))

(defn- close-popup! [popup active-trigger-atom]
  (.remove (.-classList popup) "active")
  (reset! active-trigger-atom nil))

(defn- open-popup! [popup trigger x y active-trigger-atom]
  (when-not (.-parentNode popup)
    (.appendChild js/document.body popup))
  (set! (.-innerHTML popup) "")
  (let [triples (parse-backlinks trigger)
        list-el (build-backlinks-list triples)]
    (.appendChild popup list-el)

    ;; Make visible first so offsetWidth / offsetHeight can be measured.
    (.add (.-classList popup) "active")

    (position-popup! popup x y)

    (reset! active-trigger-atom trigger)))

(defn init []
  (let [popup (ensure-popup!)
        active-trigger (atom nil)
        close! #(close-popup! popup active-trigger)]

    (.addEventListener js/window "scroll" close! #js {:passive true})

    (.addEventListener js/document "click"
      (fn [e]
        (let [target (.-target e)
              trigger (extract-trigger target ".button-popup-backlinks") ;
              is-open? (.contains (.-classList popup) "active")
              is-same-trigger? (and trigger is-open? (= @active-trigger trigger))]
          (cond
            ;; Close popup if clicking on the active trigger.
            is-same-trigger?
            (do (.preventDefault e)
                (.stopPropagation e)
                (close!))

            ;; Close popup if clicking on an inactive trigger.
            trigger
            (do (.preventDefault e)
                (.stopPropagation e)
                (open-popup! popup trigger (.-clientX e) (.-clientY e) active-trigger))

            ;; Close popup if clicking anywhere outside.
            (and (.-parentNode popup) (not (.contains popup target)))
            (close!)))))))
