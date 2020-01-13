(ns hc-scraper.trello
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(def ^:private api-url "https://api.trello.com/1")

(def trello-data (clojure.edn/read-string (slurp "trello.edn")))

(def auth (select-keys trello-data [:key :token]))
(def board-id (:board-id trello-data))
(def list-id (:list-id trello-data))


(defn ^:private trello-get
  [path-seq params]
  (let [response @(http/request
                    {:method       :get
                     :url          (string/join "/" (cons api-url path-seq))
                     :query-params (merge params auth)})]
    (when (= 200 (:status response))
      (json/read-str (:body response) :key-fn keyword))))


(defn ^:private trello-post
  [path-seq params]
  (let [response @(http/request
                    {:method      :post
                     :url         (string/join "/" (cons api-url path-seq))
                     :form-params (merge params auth)})]
    (when (= 200 (:status response))
      (json/read-str (:body response) :key-fn keyword))))


(defn all-labels
  "Retrieves all labels that exist on the board, as a map from name to id."
  []
  (let [labels (trello-get ["boards" board-id "labels"] nil)]
    (reduce
      #(assoc %1 (:name %2) (:id %2))
      {}
      labels)))


(defn create-label
  "Creates a label with the given name and color if it does not exist yet.
  Returns the id of the created/retrieved label."
  [name color]
  (:id (trello-post
         ["labels"]
         {:idBoard board-id
          :name    name
          :color   color})))


(defn ^:private create-card
  [name description image-url label-ids]
  (trello-post
    ["cards"]
    {:idList    list-id
     :name      name
     :desc      description
     :urlSource image-url
     :idLabels  (string/join "," label-ids)}))


(defn ^:private add-comment
  [card-id text]
  (trello-post
    ["cards" card-id "actions" "comments"]
    {:text text}))


(defn upload
  "Creates a new card in my Humble Games Trello-board"
  [title description image-url yt-url label-ids]
  (when-let [card (create-card title description image-url label-ids)]
    (add-comment (:id card) yt-url)
    :ok))