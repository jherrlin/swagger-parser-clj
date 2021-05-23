(ns se.jherrlin.swagger.specs.swagger-v2
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))


(s/def ::non-blank-string (s/and string? (complement str/blank?)))

(s/def ::in ::non-blank-string)
(s/def ::name ::non-blank-string)
(s/def ::format ::non-blank-string)
(s/def ::description ::non-blank-string)
(s/def ::type ::non-blank-string)
(s/def ::required boolean?)
(s/def ::parameter
  (s/keys :req-un [::in
                   ::name
                   ::required]
          :opt-un [::format
                   ::description
                   ::type]))
(s/def ::parameters
  (s/coll-of ::parameter))

(s/def ::summary string?)
(s/def ::description string?)
(s/def ::action-detail
  (s/keys :req-un [::parameters]
          :opt-un [::summary
                   ::description]))

(s/def ::http-method #{:get :put :post :delete})
(s/def ::http-actions
  (s/map-of ::http-method ::action-detail))

(s/def ::url keyword?)

(s/def ::paths
  (s/map-of ::url ::http-actions))

(s/def ::swagger-api-2.0
  (s/keys :req-un [::paths]))
