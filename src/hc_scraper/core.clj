(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hc-scraper.trello :as trello]
            [hc-scraper.web :refer [load-hiccup parse-html search-all search]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.time LocalDateTime)))


(def trello-data (clojure.edn/read-string (slurp "trello-humble-board-data.edn")))
(def board-id (:board-id trello-data))
(def upload-list-id (:upload-list-id trello-data))


(defn ^:private print-flush [& args]
  (apply print args)
  (flush))


(defn ^:private fuzzy-title
  "Replaces some known culprit characters with there expected variants"
  [s]
  (-> (string/lower-case s)
      (string/replace \– \-)))

(defn ^:private extract-game-title
  [candidate-row]
  (->> (search candidate-row :span {:class "title"})
       (drop 2)
       (filter string?)
       (map string/trim)
       (remove string/blank?)
       first))

(defn ^:private matches-title?
  [title candidate-row]
  (let [candidate-title (extract-game-title candidate-row)]
    (or (= candidate-title title)
        (= (fuzzy-title candidate-title)
           (fuzzy-title title)))))

(defn ^:private fetch-steam-url
  "Queries Steam for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (print-flush " - Searching for Steam Page URL... ")
  (let [clean-title (or (second (re-matches #"(.*?)( \+ [\w ]+ DLC[s*]?)$" title))
                        title)
        encoded-title (java.net.URLEncoder/encode ^String clean-title "UTF-8")
        html (load-hiccup (str "https://store.steampowered.com/search/?category1=998&term=" encoded-title))
        candidates (search-all html :a {:data-search-page "1"} true)
        match (->> candidates
                   (filter #(matches-title? clean-title %))
                   first)
        best-guess (->> candidates
                        (filter #(let [name (extract-game-title %)]
                                   (string/starts-with?
                                     (string/lower-case name)
                                     (string/lower-case clean-title))))
                        first)
        easy-guess (first candidates)
        [_ {:keys [href]}] (or match best-guess easy-guess)]
    (println (cond
               match "OK"
               best-guess "Good guess"
               easy-guess "Bad guess"
               :else "Failed"))
    (if href
      (md/link
        (str "Steam Page" (when-not match " ?"))
        (subs href 0 (string/last-index-of href "/")))
      (md/link "Steam Page NOT FOUND" ""))))


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
                     (md/link "Youtube Trailer NOT FOUND" nil))
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



(defn transfer-cards-from-upload-list!
  []
  (let [list-ids (->> (trello/all-lists board-id)
                      (filter #(= 1 (count (:name %))))
                      (map (juxt (comp first string/lower-case :name)
                                 :id))
                      (into {}))
        ; add fake keys for digits
        list-ids (->> (range 10)
                      (map (juxt (comp first str)
                                 (constantly (list-ids \#))))
                      (into list-ids))
        find-target-list (fn [{:keys [name]}]
                           (-> (trello/make-sortable name)
                               (string/replace-first #"[äöü]" {"ä" "a", "ö" "o", "ü" "u"})
                               first
                               list-ids))]
    (->> (trello/get-cards upload-list-id [:name])
         (map (juxt find-target-list identity))
         (mapv #(apply trello/sort-card-into-list! %)))))


;; functions for REPL evaluation
(comment

  (defn fetch! [upload-to-trello?]
    (let [now (LocalDateTime/now)
          month (-> now .getMonth .name .toLowerCase)
          year (-> now .getYear)]
      (process-url!
        (str "https://www.humblebundle.com/subscription/" month "-" year)
        upload-to-trello?)
      (trello/sort-list! upload-list-id)))

  ;; load humble choice games for the given month and year
  (fetch! true)

  (fetch! false)

  ;; sorts the "NEW" list
  (trello/sort-list! upload-list-id)

  ;; delete all cards in "NEW"
  (->> (trello/get-cards upload-list-id [:id])
       (map :id)
       (mapv trello/delete-card!)
       empty?)

  ;; move cards from "NEW" to lists
  (transfer-cards-from-upload-list!)

  ;; sort all lists
  (trello/sort-all-lists! board-id)
  )
