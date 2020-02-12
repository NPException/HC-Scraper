(ns hc-scraper.trello
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:import [java.text Collator]
           [java.util Locale Date]
           [java.util.concurrent LinkedBlockingQueue]))

(def ^:private api-url "https://api.trello.com/1")


(def ^:private auth
  (select-keys
    (clojure.edn/read-string (slurp "trello-auth.edn"))
    [:key :token]))



(defonce rate-limit (atom 0))
(def rate-limit-sleep 200)

(defonce ^LinkedBlockingQueue request-queue (LinkedBlockingQueue.))
(defonce request-runner
         (future
           (while true
             (let [request-fn (.take request-queue)]
               (when (> @rate-limit 0)
                 (Thread/sleep rate-limit-sleep)
                 (swap! rate-limit dec))
               (request-fn)))))



(defn ^:private api-request
  [method param-type parse-response? async? path-seq params]
  (let [url (string/join "/" (cons api-url path-seq))
        response (promise)
        make-request (fn execute-request []
                       (http/request
                         {:method    method
                          :url       url
                          param-type (merge params auth)}
                         (fn [result]
                           (when (or (case (:status result)
                                       (200 429) false
                                       true)
                                     (:error result))
                             (println (str "Trello returned error for request to " method " " url
                                           " -> status: " (:status result) ", error: " (:error result))))
                           ;; check for rate limiting
                           (if (= 429 (:status result))
                             (do (println (Date.) "Hit rate limit. Retry after short wait." method url)
                                 (swap! rate-limit inc)
                                 (.put request-queue execute-request))
                             (deliver response result)))))]
    (.put request-queue make-request)
    (when-not async?
      (let [success? (= 200 (:status @response))]
        (if (and parse-response? success?)
          (json/read-str (:body @response) :key-fn keyword)
          success?)))))



(defn api-get
  [path-seq params]
  (api-request :get :query-params true false path-seq params))

(defn api-post
  [path-seq params]
  (api-request :post :form-params true false path-seq params))

(defn api-put
  [path-seq params async?]
  (api-request :put :form-params false async? path-seq params))

(defn api-delete
  [path-seq async?]
  (api-request :delete :query-params true async? path-seq nil))



(defn all-labels
  "Returns a list of all labels on the board."
  [board-id]
  (api-get ["boards" board-id "labels"] nil))


(def label-colors #{:yellow :purple :blue :red :green :orange :black :sky :pink :lime})

(defn create-label!
  "Creates a label with the given name and color if it does not exist yet.
  Returns the id of the created/retrieved label. Color needs to be a keyword.
  Creates a colorless label when no color is provided or the color is invaild.
  (see label-colors)"
  [board-id name color]
  (:id (api-post
         ["labels"]
         {:idBoard board-id
          :name    name
          :color   (name (or (label-colors color)
                             :null))})))


(defn create-card!
  "Creates a card with the given name in the list.
  Rest of the parameters are optional."
  [list-id name & {:keys [description image-url label-ids]}]
  (api-post
    ["cards"]
    (merge
      {:idList list-id
       :name   name}
      (when description
        {:desc description})
      (when image-url
        {:urlSource image-url})
      (when (seq label-ids)
        {:idLabels (string/join "," label-ids)}))))


(defn add-comment!
  "Adds a comment with the given markdown text to a card."
  [card-id text]
  (api-post
    ["cards" card-id "actions" "comments"]
    {:text text}))


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


(defn sort-list!
  "Sorts a list by card titles in ascending alphabetic order,
  while ignoring leading articles ('a' & 'the')."
  [list-id]
  (some->>
    (get-cards list-id [:name :id])
    (sort-by (comp make-sortable :name) (Collator/getInstance Locale/GERMANY))
    (map-indexed (fn [idx {id :id}]
                   (api-put ["cards" id] {:pos (* idx 10000)} true)))
    doall)
  nil)


(defn all-lists
  "Loads all lists from a board."
  [board-id]
  (api-get ["boards" board-id "lists"] nil))


(defn sort-all-lists!
  "Sorts all list on a board."
  [board-id]
  (->> (all-lists board-id)
       (mapv (comp sort-list! :id)))
  nil)
