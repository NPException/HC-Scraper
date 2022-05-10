(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hc-scraper.trello :as trello]
            [hc-scraper.web :as web]
            [hc-scraper.humble :as humble]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.time LocalDateTime)
           (java.net URLEncoder)))

;; TODO: maybe store cards not in alphabetical columns, but in column per bundle instead
;; TODO: add function to create a board and store id to `trello-humble-board-data.edn`


(def trello-data (clojure.edn/read-string (slurp "trello-humble-board-data.edn")))
(def board-id (:board-id trello-data))
(def upload-list-id (:upload-list-id trello-data))


(defn ^:private print-flush [& args]
  (apply print args)
  (flush))


(defn ^:private fuzzy-title
  "Replaces some known culprit characters with their expected variants"
  [s]
  (-> (string/lower-case s)
      (string/replace \– \-)))

(defn ^:private extract-game-title
  [candidate-row]
  (->> (web/search candidate-row :span {:class "title"})
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

(defn ^:private md-steam-deck-compat-badge
  [app-id]
  (md/image "Steam Deck Compatibility" (str "https://npexception.de/compatibility-badge/" app-id ".png?size=32")))

(defn ^:private extract-app-id-from-url
  [steam-url]
  (second (re-find #"/app/(\d+)" steam-url)))

(defn ^:private fetch-steam-url
  "Queries Steam for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (print-flush " - Searching for Steam Page URL... ")
  (let [clean-title (or (second (re-matches #"(.*?)( \+ [\w ]+ DLC[s*]?)$" title))
                        title)
        encoded-title (URLEncoder/encode ^String clean-title "UTF-8")
        html (web/load-hiccup (str "https://store.steampowered.com/search/?category1=998%2C994%2C21&term=" encoded-title)) ;; categories: game, software, dlc
        candidates (web/search-all html :a {:data-search-page "1"} true)
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
      [(md/link
         (str "Steam Page" (when-not match " ?"))
         (subs href 0 (string/last-index-of href "/")))
       (md-steam-deck-compat-badge (extract-app-id-from-url href))]
      [(md/link "Steam Page NOT FOUND" "")
       nil])))


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


(defn ^:private html->md [html]
  (-> (web/parse-html html)
      (web/search :body nil)
      md/as-markdown))

(defn ^:private build-md-description
  [{:keys [title genres developers trailer-url bundle-url
           description-html system-requirements-html content-type steam-app-id]
    :as _item-data}]
  (let [genre-text (some->> genres (string/join ", "))
        developers-text (some->> developers (string/join ", "))
        md-trailer-link (if trailer-url
                          (md/link "Trailer" trailer-url)
                          (md/link "Trailer NOT FOUND" nil))
        description (-> description-html html->md)
        system-requirements (some-> system-requirements-html html->md)
        [md-steam-link
         md-steam-deck-badge] (if steam-app-id
                                (do (println " - Generated Steam Page URL for known app id:" steam-app-id)
                                    [(md/link "Steam Page" (str "https://store.steampowered.com/app/" steam-app-id))
                                     (md-steam-deck-compat-badge steam-app-id)])
                                (when (#{"game" "software"} content-type)
                                  (fetch-steam-url title)))]
    (str
      (when md-steam-deck-badge
        (str (md/bold "Steam Deck Compatibility")
          md/new-section
          md-steam-deck-badge
          md/new-section
          "-----"
          md/new-section))
      (when genre-text
        (str "Genres: " (md/italic genre-text) md/new-line))
      (when developers-text
        (str "Developers: " (md/bold developers-text)))
      (when (or genre-text developers-text)
        md/new-section)
      (md/bold
        (str
          (when md-steam-link (str md-steam-link " | "))
          md-trailer-link " | " (md/link "Bundle Link" bundle-url)))
      md/new-section
      "-----"
      md/new-section
      description
      (when system-requirements
        (str
          md/new-section
          "-----"
          (md/heading 3 "System Requirements")
          system-requirements)))))


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


(defn ^:private save-to-file!
  [{:keys [title item-url-name image-url] :as _item} md-description]
  (println " - Saving markdown")
  (let [file (str "./not-uploaded/" item-url-name ".md")
        enhanced-markdown (str
                            (md/heading 1 title) "\n"
                            (md/image nil image-url) md/new-line
                            md-description)]
    (io/make-parents file)
    (spit file enhanced-markdown)))


(defn process-items!
  [items bundle-label-id upload?]
  (let [all-labels (and upload?
                        (->> (trello/all-labels board-id)
                             (map (juxt :name :id))
                             (into {})))
        find-label (and upload?
                        (memoize #(find-delivery-method-label all-labels (string/lower-case %))))]
    (doseq [item items]
      (println "Next:" (:title item))
      (let [{:keys [title image-url trailer-url]} item
            md-description (build-md-description item)
            trello-labels (and upload?
                               (cons bundle-label-id (map find-label (:delivery-methods item))))]
        (print-flush " - Uploading to Trello... ")
        (if (and upload? (upload-to-trello! title md-description image-url trailer-url trello-labels))
          (println "OK")
          (do (println (if upload? "Failed" "Skipped"))
              (save-to-file! item md-description)))))))


(defn process-humble-url!
  [humble-url extract-data-fn upload?]
  (if (nil? humble-url)
    (println "Please supply a Humble URL as parameter")
    (let [_ (println "Making request to" humble-url)
          html (web/load-hiccup humble-url)
          {:keys [items trello-label-name]} (extract-data-fn html)]
      (if (not (seq items))
        (println "No items found. Is the URL correct?")
        (do
          (println "Found" (count items) "items.")
          (let [bundle-label-id (and upload? (trello/create-label! board-id trello-label-name :red))]
            (process-items! items bundle-label-id upload?)))))))


(defn fetch-current-choice-bundle!
  "Scrapes the humble choice for the current month and year"
  [upload-to-trello?]
  (let [now (LocalDateTime/now)
        month (-> now .getMonth .name .toLowerCase)
        year (-> now .getYear)]
    (process-humble-url!
      (str "https://www.humblebundle.com/subscription/" month "-" year)
      humble/extract-choice-data
      upload-to-trello?)
    (when upload-to-trello?
      (trello/sort-list! upload-list-id))))


(defn fetch-bundle!
  "Scrapes a regular Humble bundle URL."
  [url upload-to-trello? desired-content-types]
  (process-humble-url! url #(humble/extract-bundle-data % desired-content-types) upload-to-trello?)
  (when upload-to-trello?
    (trello/sort-list! upload-list-id)))


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
         (reduce
           (fn [card-lists [list-id card]]
             (update card-lists list-id
                     #(trello/sort-card-into-list! list-id card :cards-in-list %)))
           {}))))


(defn -main [& _args]
  (transfer-cards-from-upload-list!)
  (fetch-current-choice-bundle! true))


;; functions for REPL evaluation
(comment

  (fetch-bundle! "https://www.humblebundle.com/stand-with-ukraine-bundle" true #{"game"})

  (fetch-current-choice-bundle! true)

  (fetch-current-choice-bundle! false)

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


  ;; add Steam Deck compat badges to all cards that don't have them yet

  (defn find-app-id [desc]
    (second (re-find #"\[Steam (?:Page|Link)\]\(https?://store.steampowered.com/app/(\d+)" desc)))

  (defn splice-badge [{:keys [desc] :as card}]
    (when-let [app-id (and (not (string/includes? desc "![Steam Deck Compatibility]"))
                           (find-app-id desc))]
      (assoc card :desc
        (str (md/bold "Steam Deck Compatibility")
          md/new-section
          (md-steam-deck-compat-badge app-id)
          md/new-section
          "-----"
          md/new-section
          desc))))

  (defn update-card! [card]
    (trello/api-put ["cards" (:id card)] card false))

  (let [all-lists (trello/all-lists board-id)
        all-cards (->> all-lists
                       (mapcat #(trello/get-cards (:id %) [:id :name :desc])))]
    (doseq [card all-cards]
      (print (:name card))
      (when-let [with-badge (splice-badge card)]
        (print " - UPDATED")
        (update-card! with-badge))
      (println)))
  )
