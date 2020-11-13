(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hc-scraper.trello :as trello]
            [hc-scraper.web :refer [load-hiccup parse-html search-all search]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]))


(def trello-data (clojure.edn/read-string (slurp "trello-humble-board-data.edn")))
(def board-id (:board-id trello-data))
(def upload-list-id (:upload-list-id trello-data))


(defn ^:private print-flush [& args]
  (apply print args)
  (flush))


(defn ^:private fetch-steam-url
  "Queries steamdb.info for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (print-flush " - Searching for Steam Page URL... ")
  (let [clean-title (or (second (re-matches #"(.*?)( \+ \d+ DLCs?)$" title))
                        title)
        encoded-title (java.net.URLEncoder/encode ^String clean-title "UTF-8")
        html (load-hiccup (str "https://steamdb.info/search/?a=app&type=1&category=0&q=" encoded-title))
        candidate (search-all html :tr {:class "app"} true)
        match (->> candidate
                   (filter #(contains? (set %) [:td {} clean-title]))
                   first)
        best-guess (->> candidate
                        ;; game name is at index 7 in :tr element
                        (filter #(let [[_ _ name] (nth % 7)]
                                   (string/starts-with?
                                     (string/lower-case name)
                                     (string/lower-case clean-title))))
                        first)
        easy-guess (first candidate)
        [_ {:keys [data-appid]}] (or match best-guess easy-guess)]
    (println (if data-appid "OK" "Failed"))
    (if data-appid
      (md/link
        (str "Steam Page" (when-not match " (?)"))
        (str "https://store.steampowered.com/app/" data-appid))
      (md/link "Steam Page (NOT FOUND)" ""))))


(defn ^:private upload-to-trello!
  "Creates a new card in my Humble Games Trello-board"
  [title description image-url yt-url label-ids]
  (when-let [card (trello/create-card! upload-list-id title
                                       :description description
                                       :image-url image-url
                                       :label-ids label-ids)]
    (when yt-url
      (trello/add-comment! (:id card) yt-url))
    :ok))


(defn ^:private process-game! [data trello-labels]
  (let [title (:title data)
        _ (println "Next:" title)
        genres (string/join ", " (:genres data))
        developers (string/join ", " (:developers data))
        yt-url (some->> data :carousel_content :youtube-link first
                        (str "https://www.youtube.com/watch?v="))
        md-yt-link (if yt-url
                     (md/link "Youtube Trailer" yt-url)
                     (md/link "Youtube Trailer (NOT FOUND)" nil))
        choice-url (:choice-url data)
        image-url (:image data)
        description (-> (:description data)
                        parse-html
                        (search :body nil)
                        md/as-markdown)
        system-requirements (some-> (:system_requirements data)
                                    parse-html
                                    (search :body nil)
                                    md/as-markdown)
        md-steam-link (fetch-steam-url title)
        description-md (str
                         "Genres: " (md/italic genres) md/new-line
                         "Developers: " (md/bold developers)
                         md/new-section
                         (md/bold
                           (str md-steam-link " | " md-yt-link " | " (md/link "Humble Choice Link" choice-url)))
                         md/new-section
                         "-----"
                         md/new-section
                         description
                         (when system-requirements
                           (str
                             md/new-section
                             "-----"
                             (md/heading 3 "System Requirements")
                             system-requirements)))]

    (print-flush " - Uploading to Trello... ")
    (if (and trello-labels (upload-to-trello! title description-md image-url yt-url trello-labels))
      (println "OK")
      (do (println "Failed")
          (println " - Saving markdown")
          (let [file (str "./not-uploaded/" (:game-url-name data) ".md")
                enhanced-markdown (str
                                    (md/heading 1 title) "\n"
                                    (md/image image-url) md/new-line
                                    description-md)]
            (io/make-parents file)
            (spit file enhanced-markdown))))))


(defn ^:private create-delivery-method-label
  [delivery-method]
  (trello/create-label! board-id (string/capitalize delivery-method) :sky))

(defn ^:private find-delivery-method-label
  [labels delivery-method]
  (if-let [[_ id] (->> labels
                       (filter
                         #(-> (key %)
                              string/lower-case
                              (string/includes? delivery-method)))
                       first)]
    id
    (create-delivery-method-label delivery-method)))


(defn ^:private one-of [m & keys]
  (when keys
    (or (get m (first keys))
        (recur m (next keys)))))


(defn process-url!
  [choice-month-url upload?]
  (if (nil? choice-month-url)
    (println "Please supply at least one humble choice month URL as parameter")
    (let [_ (println "Making request to" choice-month-url)
          html (load-hiccup choice-month-url)
          [_ _ json-data] (search html :script {:id "webpack-monthly-product-data"} true)
          raw-edn (-> json-data
                      (json/read-str :key-fn keyword))
          data (:contentChoiceOptions raw-edn)
          base-url (str "https://www.humblebundle.com/subscription/" (:productUrlPath data) "/")
          choice-month (:title data)
          games (-> data
                    :contentChoiceData
                    (one-of :initial :initial-without-order)
                    :content_choices)]
      (if (not games)
        (println "No Humble Choice games found. Is the URL correct?")
        (do
          (println "Found" (count games) "games.")
          (let [month-label-id (and upload?
                                    (trello/create-label! board-id (str "HC " choice-month) :red))
                all-labels (and upload?
                                (->> (trello/all-labels board-id)
                                     (map (juxt :name :id))
                                     (into {})))
                find-label (and upload?
                                (memoize #(find-delivery-method-label all-labels %)))]
            (doseq [game games]
              (let [game-url-name (name (key game))
                    game-data (assoc (val game)
                                :game-url-name game-url-name
                                :choice-url (str base-url game-url-name))
                    trello-labels (and upload?
                                       (cons month-label-id (map find-label (:delivery_methods game-data))))]
                (process-game! game-data trello-labels)))))))))


(defn -main [& [choice-url]]
  (process-url! choice-url true))


;; functions for REPL evaluation
(comment

  ;; load humble choice games for the given month and year
  (let [now (LocalDateTime/now)
        month (-> now .getMonth .name .toLowerCase)
        year (-> now .getYear)
        upload-to-trello? true]
    (process-url! (str "https://www.humblebundle.com/subscription/" month "-" year) upload-to-trello?))

  ;; sorts the "NEU" list
  (trello/sort-list! upload-list-id)

  ;; delete all cards in "NEU"
  (->> (trello/get-cards upload-list-id [:id])
       (map :id)
       (mapv trello/delete-card!)
       empty?)

  ;; sort all lists
  (trello/sort-all-lists! board-id)
  )


;; TODO: Transfer cards from "NEU" to their apropriate lists,
;; TODO:  either into their sorted position, or sort the list afterwards.
