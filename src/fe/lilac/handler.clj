(ns lilac.handler
  (:require [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [clojure.string :as str]))

(defn wrap-autorefresh [handler]
  (fn [request]
    (let [response (handler request)]
      (if (and (= 200 (:status response)) ;
               (clojure.string/includes?
                 (get-in response [:headers "Content-Type"] "") "text/html"))
        (let [body-str (slurp (:body response))
              script
              "<script>
                (function() {
                  const scrollSelectors = ['nav.toc', 'main', 'aside.sidebar']; //
                  const storageKey = 'dev-scroll-map';

                  let lastScrollTime = 0;
                  window.addEventListener('scroll', () => { //
                    lastScrollTime = Date.now();
                  }, true);

                  function setScrollPositions(map) {
                    for (let sel in map) {
                      let el = document.querySelector(sel);
                      if (el) el.scrollTop = map[sel];
                    }
                  }

                  function getScrollPositions(selectors) {
                    let map = {};
                    selectors.forEach(sel => {
                      let el = document.querySelector(sel);
                      if (el) map[sel] = el.scrollTop;
                    });
                    return map;
                  }

                  const rawData = localStorage.getItem(storageKey);
                  if (rawData) {
                    const prevScrollPositions = JSON.parse(rawData);
                    setTimeout(() => {
                      setScrollPositions(prevScrollPositions); //
                      localStorage.removeItem(storageKey);
                    }, 200); //
                  }

                  function refreshPage() {
                    const isPaused = localStorage.getItem('dev-pause-refresh') === 'true';
                    const recentlyScrolled = (Date.now() - lastScrollTime) < 1500; //

                    if (isPaused || recentlyScrolled) { //
                      setTimeout(refreshPage, 500); //
                      return;
                    }

                    const currentScrollPositions = getScrollPositions(scrollSelectors);
                    localStorage.setItem(storageKey,
                                         JSON.stringify(currentScrollPositions)); //

                    window.location.reload(); //
                  }

                  setTimeout(refreshPage, 1000); //
                })();
              </script>"]
          (-> response
              (assoc :body
                     (clojure.string/replace body-str "</body>"
                                             (str script "</body>")))
              (update :headers dissoc "Content-Length")))
        response))))

(def development-app
  (-> (constantly {:status 404 :headers {} :body "Not Found"}) ;
      (wrap-file ".") ;
      (wrap-content-type) ;
      (wrap-not-modified) ;
      (wrap-autorefresh)))
