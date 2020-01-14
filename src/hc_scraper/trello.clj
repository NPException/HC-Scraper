(ns hc-scraper.trello
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:import [java.text Collator]
           [java.util Locale Date]))

(def ^:private api-url "https://api.trello.com/1")

(def trello-data (clojure.edn/read-string (slurp "trello.edn")))

(def auth (select-keys trello-data [:key :token]))
(def board-id (:board-id trello-data))
(def list-id (:list-id trello-data))

;; TODO: create request queue, to re-queue rate-limited requests without starving the http-kit worker pool

(defn ^:private trello-request
  [method param-type parse-response? async? path-seq params]
  (let [url (string/join "/" (cons api-url path-seq))
        make-request (fn make-request [retry?]
                       (http/request
                         {:method    method
                          :url       url
                          param-type (merge params auth)
                          :deadlock-guard? false}
                         (fn [result]
                           (when (or (case (:status result)
                                       (200 429) false
                                       true)
                                     (:error result))
                             (println (str "Trello returned error for request to " method " " url
                                           " -> status: " (:status result) ", error: " (:error result))))
                           (if retry?
                             (loop [result result
                                    backoff (concat [150 300 500 1000] (repeatedly #(+ 2000 (rand-int 2000))))]
                               ;; check for rate limiting
                               (if (= 429 (:status result))
                                 (do (println (Date.) "Ran into Trello's rate limit. Wait:" (first backoff) method url)
                                     (Thread/sleep (first backoff))
                                     (recur @(make-request false) (next backoff)))
                                 result))
                             result))))
        response (make-request true)]
    (when-not async?
      (let [success? (= 200 (:status @response))]
        (if (and parse-response? success?)
          (json/read-str (:body @response) :key-fn keyword)
          success?)))))


(defn ^:private trello-get
  [path-seq params]
  (trello-request :get :query-params true false path-seq params))

(defn ^:private trello-post
  [path-seq params]
  (trello-request :post :form-params true false path-seq params))

(defn ^:private trello-put
  [path-seq params async?]
  (trello-request :put :form-params false async? path-seq params))


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


(defn ^:private trim-articles
  [name]
  (cond
    (string/starts-with? name "the ") (subs name 4)
    (string/starts-with? name "a ") (subs name 2)
    :else name))

(defn ^:private make-sortable
  [title]
  (->> title
       string/lower-case
       string/trim
       trim-articles
       (re-seq #"\w|[äöü]")
       string/join))

(defn ^:private sort-list!
  "Sort all cards in a list"
  [list-id]
  (some->>
    (trello-get ["lists" list-id "cards"] {:fields "name,id"})
    (sort-by (comp make-sortable :name) (Collator/getInstance Locale/GERMANY))
    (map-indexed (fn [idx {id :id}]
                   (Thread/sleep 100)
                   (trello-put ["cards" id] {:pos (* idx 10000)} true)))
    doall)
  nil)

(defn ^:private all-lists
  "Loads all list from the board"
  []
  (-> (trello-get ["boards" board-id "lists"] nil)))

(defn ^:private sort-all-lists!
  "Sorts all list in the board"
  []
  (->> (all-lists)
       (mapv (comp sort-list! :id)))
  :done)

;; TODO: Transfer cards from "NEU" to their apropriate lists,
;; TODO:  either into their sorted position, or sort the list afterwards.
