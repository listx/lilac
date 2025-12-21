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
                  const parser = new DOMParser();
                  let mainEl = null;
                  let resp = null;
                  let html = null;
                  let doc = null;
                  let currentScroll = 0;

                  let lastScrollTime = 0;
                  window.addEventListener('scroll', function(e) {
                    lastScrollTime = Date.now();
                  }, true);

                  async function softRefreshLoop() {
                    try {
                      if (Date.now() - lastScrollTime < 1500) { //
                        return;
                      }

                      mainEl = document.querySelector('main');
                      currentScroll = mainEl.scrollTop; //

                      resp = await fetch(window.location.href);
                      html = await resp.text();
                      doc = parser.parseFromString(html, 'text/html');
                      document.body.innerHTML = doc.body.innerHTML;

                      mainEl = document.querySelector('main');
                      mainEl.scrollTop = currentScroll; //
                    } catch (e) {
                      console.error('Soft refresh failed', e);
                    } finally {
                      setTimeout(softRefreshLoop, 1000);
                    }
                  }

                  softRefreshLoop();
                })();
              </script>"]
          (-> response
              (assoc :body
                     (clojure.string/replace body-str "</body>"
                                             (str script "</body>")))
              (update :headers dissoc "Content-Length")))
        response))))

(def development-app
  (-> (constantly {:status 404 :headers {} :body "Not Found"})

      (wrap-file ".")
      (wrap-content-type) ;
      (wrap-not-modified) ;
      (wrap-autorefresh)))
