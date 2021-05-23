(ns se.jherrlin.swagger
  (:require [clojure.data.json :as json]
            [clojure.core :refer [fn]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [replace]]
            [se.jherrlin.swagger.specs.swagger-v1 :as swagger-v1]
            [se.jherrlin.swagger.specs.swagger-v2 :as swagger-v2]))


(defn json->edn
  "Read JSON file and parse it to EDN."
  [json-str]
  (json/read-str json-str :key-fn keyword))

(defn sanitize-url [s]
  (replace s #"^:" ""))

(defn doc-group-params [xs]
  (reduce (fn [s {:keys [name required description type schema]}]
            (let [help (cond
                         description description
                         type        type
                         schema      (str (:type schema) ", " (-> schema :items :type)))]
              (str s (format "%-3s" (if required "*" "")) "- `" (format "%-30s" (str name "`")) "- "help "\n")))
          ""
          xs))

(defn doc-str-path-and-query-params [parameters]
  (->> parameters
       (group-by :param-type)
       (reduce
        (fn [s [group params]]
          (str s " " group "\n"
               (doc-group-params params)))
        "")))

(defn docs-str-file-links [file-links]
  (str "Source code files found:\n"
       (reduce (fn [s x]
                 (str s x "\n"))
               ""
               file-links)))

(defn doc-string
  "Convert the parameters into a docstring.

  Docstring tells what parameters that could be used.
  * indicates that the parameter is required."
  [{:keys [parameters url file-links original-swagger-source summary]}]
  {:pre  [(s/valid? coll? parameters)]
   :post [#(s/valid? string? %)]}
  (let [url-str (replace url #"^:" "")]
    (str "\n"
         (when summary
           (str summary "\n\n"))
         "Docs on url: " url-str
         "\n\n`*` is required ones\n\n"
         (doc-str-path-and-query-params parameters)
         "\n"
         (when (seq file-links)
           (str
            "\n"
            (docs-str-file-links file-links)))
         "\n"
         (str "Endpoint defined in: \n" original-swagger-source))))

(defn add-doc-string [m]
  (assoc m :docstring (doc-string m)))

(defn gen-query-params-fn [parameters]
  (let [args (->> parameters
                  (filter (comp #{"query"} :param-type))
                  (mapv (comp symbol :name)))]
    (if-not (seq args)
      `(clojure.core/fn [{:keys []}] {})
      `(clojure.core/fn [{:keys ~args}]
         ~(reduce (fn [m x] (assoc m (keyword x) x)) {} args)))))

(defn add-query-params-fn [{:keys [parameters] :as m}]
  (assoc m :query-params-fn (gen-query-params-fn parameters)))

(defn gen-url-fn
  "Generate url-fn from url string."
  [url-str]
  (let [args       (->> (re-seq #"(?i)(\{[a-z]+\})" url-str)
                        (mapv (comp symbol #(replace % #"\{|\}" "") second)))
        format-str (str/replace url-str #"(?i)\{[a-z]+\}" "%s")]
    (if-not (seq args)
      `(clojure.core/fn [{:keys []}] ~url-str)
      `(clojure.core/fn [{:keys ~args}]
         (clojure.core/apply clojure.core/format ~format-str ~args)))))

(defn add-all-arguments [{:keys [parameters] :as m}]
  (assoc m :all-arguments (mapv :name parameters)))

(defn add-url-fn [{:keys [url] :as m}]
  (assoc m :url-fn (gen-url-fn url)))

(defmulti swagger-normalizer ::swagger-version)

(defmethod swagger-normalizer :default
  [{:keys [original-swagger-source]}]
  (throw (Exception. (str "Dont know how to parse this source: " original-swagger-source))))

(defmethod swagger-normalizer ::version2.0
  [{:keys [original-swagger-source] :as swagger-edn}]
  (->> swagger-edn
       :paths
       (mapcat
        (fn [[url http-actions]]
          (map (fn [[http-method {:keys [parameters summary description]}]]
                 {:url                     (sanitize-url url)
                  :original-swagger-source original-swagger-source
                  :http-method             (-> http-method name str/upper-case keyword)
                  :summary                 (or summary description)
                  :parameters              (mapv (fn [{:keys [in name required format description type]}]
                                                   {:param-type  in
                                                    :name        name
                                                    :required    required
                                                    :format      type
                                                    :description description})
                                                 parameters)})
               http-actions)))))

(defmethod swagger-normalizer ::version1.2
  [{:keys [original-swagger-source] :as xs}]
  (->> xs
       :apis
       (mapcat
        (fn [{:keys [path operations]}]
          (map
           (fn [{:keys [method summary parameters]}]
             {:url                     path
              :original-swagger-source original-swagger-source
              :http-method             (-> method str/upper-case keyword)
              :summary                 summary
              :parameters              (mapv (fn [{:keys [paramType description name format]}]
                                               {:param-type  paramType
                                                :description description
                                                :name        name
                                                :format      format
                                                :required    nil})
                                             parameters)})
           operations)))))

(defn ->swagger-edn [[swagger-source json-str]]
  (let [swagger-edn (json->edn json-str)]
    (cond-> (assoc swagger-edn :original-swagger-source swagger-source)
      (s/valid? ::swagger-v2/swagger-api-2.0 swagger-edn) (assoc ::swagger-version ::version2.0)
      (s/valid? ::swagger-v1/swagger-api-1.2 swagger-edn) (assoc ::swagger-version ::version1.2))))

(defn read-source [source]
  ((juxt identity slurp) source))

(defn endpoints [swagger-sources]
  (->> swagger-sources
       (map read-source)
       (map ->swagger-edn)
       (mapcat swagger-normalizer)
       (map add-url-fn)
       (map add-query-params-fn)
       (map add-all-arguments)
       (map add-doc-string)))
