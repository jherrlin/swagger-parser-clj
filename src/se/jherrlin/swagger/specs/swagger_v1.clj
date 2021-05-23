(ns se.jherrlin.swagger.specs.swagger-v1
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))


(s/def ::non-blank-string (s/and string? (complement str/blank?)))

(s/def ::format #{"date" "int64" "uuid"})
(s/def ::name ::non-blank-string)
(s/def ::description ::non-blank-string)
(s/def ::paramType #{"query" "path" "body" "header"})
(s/def ::parameter
  (s/keys :req-un [::paramType
                   ::description
                   ::name]
          :opt-un [::format]))

(s/def ::parameters (s/coll-of ::parameter))
(s/def ::summary ::non-blank-string)
(s/def ::method #{"GET" "PUT" "POST" "DELETE"})
(s/def ::action
  (s/keys :req-un [::method
                   ::summary
                   ::parameters]))

(s/def ::operations (s/coll-of ::action))
(s/def ::path ::non-blank-string)

(s/def ::api
  (s/keys :req-un [::path
                   ::operations]))

(s/def ::apis
  (s/coll-of ::api))

(s/def ::swagger-api-1.2
  (s/keys :req-un [::apis]))
