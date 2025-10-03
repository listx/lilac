(ns lilac.handler
  (:require [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.content-type :refer [wrap-content-type]]))

(def development-app
  (-> (constantly {:status 404 :headers {} :body "Not Found"})
      (wrap-content-type)
      (wrap-not-modified)
      (wrap-file ".")))
