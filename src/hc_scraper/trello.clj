(ns hc-scraper.trello
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:import (java.text Collator)
           (java.util Locale Date)
           (java.util.concurrent LinkedBlockingQueue)))

;; TODO: split bare API functions and utility functions (like sorting) into separate namespaces

(def ^:private api-url "https://api.trello.com/1")


(def ^:private auth
  (select-keys
    (clojure.edn/read-string (slurp "trello-auth.edn"))
    [:key :token]))



(defonce rate-limit (atom 0))
(def ^Long rate-limit-sleep 200)

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
                           (when (or (case (int (or (:status result) 0))
                                       (200 429) false
                                       true)
                                     (:error result))
                             (println (str "Trello returned error for request to " method " " url
                                           " -> status: " (prn-str (:status result)) ", error: " (prn-str (:error result)))))
                           ;; check for rate limiting
                           (if (= 429 (:status result))
                             (do (println (Date.) "Hit rate limit. Retry after short wait." method url)
                                 (swap! rate-limit inc)
                                 (.put request-queue execute-request))
                             (deliver response result)))))]
    (.put request-queue make-request)
    ((if async? identity deref)
     (delay
       (let [success? (= 200 (:status @response))]
         (if (and parse-response? success?)
           (json/read-str (:body @response) :key-fn keyword)
           success?))))))



(defn api-get
  [path-seq params]
  (api-request :get :query-params true false path-seq params))

(defn api-post
  [path-seq params]
  (api-request :post :form-params true false path-seq params))

(defn api-put
  [path-seq params async?]
  (api-request :put :form-params true async? path-seq params))

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
  [board-id label-name color]
  (:id (api-post
         ["labels"]
         {:idBoard board-id
          :name    label-name
          :color   (name (or (label-colors color)
                             :null))})))


(defn get-card
  "Gets the card with the given id, optionally only returning the desired fields.
  Fields can be passed as a symbols, keywords or strings."
  ([card-id]
   (get-card card-id nil))
  ([card-id fields]
   (api-get ["cards" card-id ]
     (when (seq fields)
       {:fields (->> fields
                     (map name)
                     (string/join ","))}))))


(defn get-cards
  "Gets the cards in the given list, optionally only returning the desired fields.
  Fields can be passed as a symbols, keywords or strings."
  ([list-id]
   (get-cards list-id nil))
  ([list-id fields]
   (api-get ["lists" list-id "cards"]
            (when (seq fields)
              {:fields (->> fields
                            (map name)
                            (string/join ","))}))))


(defn ^:private trim-articles
  [name]
  (cond
    (string/starts-with? name "the ") (subs name 4)
    (string/starts-with? name "a ") (subs name 2)
    :else name))


(defn make-sortable
  "Transforms the given title to a format that can be used for alphabetical sorting."
  ^String [title]
  (->> title
       string/lower-case
       string/trim
       trim-articles
       (re-seq #"\w|[äöü]")
       string/join))


(def ^:private collator (Collator/getInstance Locale/GERMANY))

(defn ^:private compare-cards-by-name
  [{name-1 :name} {name-2 :name}]
  (.compare ^Collator collator
            (make-sortable name-1)
            (make-sortable name-2)))


(defn ^:private difference
  [n1 n2]
  (if (> n1 n2)
    (- n1 n2)
    (- n2 n1)))


(defn ^:private determine-position
  "Calculates the position a card with the given name
  would take in the desired list. This is assuming that the cards
  in the Trello list are already alphabetically sorted."
  [cards card]
  (let [cards (sort-by :pos cards)
        first-card (first cards)
        last-card (last cards)]
    (cond
      (empty? cards) 0

      ;; less than first card
      (< (compare-cards-by-name card first-card) 0)
      "top"

      ;; more than last card
      (>= (compare-cards-by-name card last-card) 0)
      "bottom"

      :somewhere-else
      (->> cards
           (map #(vector (compare-cards-by-name card %) (:pos %)))
           (partition 2 1)
           (map (fn [[[c1 pos1] [c2 pos2]]]
                  (when (not= c1 c2)
                    (+ pos1 (/ (difference pos1 pos2) 2.0)))))
           (filter some?)
           first))))


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


(defn delete-card!
  [card-id]
  (api-delete ["cards" card-id] true))


(defn archive-card!
  [card-id]
  (api-put ["cards" card-id] {:closed true} false))


(defn add-comment!
  "Adds a comment with the given markdown text to a card."
  [card-id text]
  (api-post
    ["cards" card-id "actions" "comments"]
    {:text text}))


(defn sort-list!
  "Sorts a list by card titles in ascending alphabetic order,
  while ignoring leading articles ('a' & 'the')."
  [list-id]
  (some->>
    (get-cards list-id [:name :id])
    (sort compare-cards-by-name)
    (map-indexed (fn [idx {id :id}]
                   (api-put ["cards" id] {:pos (* (inc idx) 10000)} true)))
    doall)
  nil)


(defn sort-card-into-list!
  "Moves a given card into the desired list.
  If the cards in the target list are supplied via 'cards-in-list'
  (with :name and :pos fields), they will be used to determine the new position,
  instead of querying Trello for the cards in the list."
  [list-id card & {:keys [cards-in-list]}]
  (let [cards-in-list (or cards-in-list (get-cards list-id [:name :pos]))
        new-data {:idList list-id
                  :pos    (determine-position cards-in-list card)}
        updated-card (api-put ["cards" (:id card)] new-data false)]
    (conj cards-in-list
          (assoc card :pos (:pos updated-card)))))


(defn get-list
  "Gets the list with the given id, optionally only returning the desired fields.
  Fields can be passed as a symbols, keywords or strings."
  ([list-id]
   (get-list list-id nil))
  ([list-id fields]
   (api-get ["lists" list-id]
            (when (seq fields)
              {:fields (->> fields (map name) (string/join ","))}))))


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


(defn all-cards
  "Loads all cards from a board"
  ([board-id]
   (all-cards board-id nil))
  ([board-id fields]
   (->> (all-lists board-id)
        (sequence
          (comp (map #(get-cards (:id %) fields))
                cat)))))
