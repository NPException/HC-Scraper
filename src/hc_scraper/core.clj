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

;; TODO: automatically use first Trello list as NEW list
;; TODO: create target list (#,A-Z) automatically when necessary and not yet present
;; TODO: add function to create a board and store id to `trello-humble-board-data.edn`


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

(defn ^:private fetch-steam-url
  "Queries Steam for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (print-flush " - Searching for Steam Page URL... ")
  (let [clean-title (or (second (re-matches #"(.*?)( \+ [\w ]+ DLC[s*]?)$" title))
                        title)
        encoded-title (URLEncoder/encode ^String clean-title "UTF-8")
        html (web/load-hiccup (str "https://store.steampowered.com/search/?category1=998&term=" encoded-title))
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


(defn ^:private html->md [html]
  (-> (web/parse-html html)
      (web/search :body nil)
      md/as-markdown))

(defn ^:private build-md-description
  [{:keys [title genres developers trailer-url bundle-url
           description-html system-requirements-html]
    :as _game-data}]
  (let [genre-text (some->> genres (string/join ", "))
        developers-text (some->> developers (string/join ", "))
        md-trailer-link (if trailer-url
                          (md/link "Trailer" trailer-url)
                          (md/link "Trailer NOT FOUND" nil))
        description (-> description-html html->md)
        system-requirements (some-> system-requirements-html html->md)
        md-steam-link (fetch-steam-url title)]
    (str
      (when genre-text
        (str "Genres: " (md/italic genre-text) md/new-line))
      (when developers-text
        (str "Developers: " (md/bold developers-text)))
      (when (or genre-text developers-text)
        md/new-section)
      (md/bold
        (str md-steam-link " | " md-trailer-link " | " (md/link "Bundle Link" bundle-url)))
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
  [{:keys [title game-url-name image-url] :as _game} md-description]
  (println " - Saving markdown")
  (let [file (str "./not-uploaded/" game-url-name ".md")
        enhanced-markdown (str
                            (md/heading 1 title) "\n"
                            (md/image image-url) md/new-line
                            md-description)]
    (io/make-parents file)
    (spit file enhanced-markdown)))


(defn process-games!
  [games bundle-label-id upload?]
  (let [all-labels (and upload?
                        (->> (trello/all-labels board-id)
                             (map (juxt :name :id))
                             (into {})))
        find-label (and upload?
                        (memoize #(find-delivery-method-label all-labels %)))]
    (doseq [game games]
      (println "Next:" (:title game))
      (let [{:keys [title image-url trailer-url]} game
            md-description (build-md-description game)
            trello-labels (and upload?
                               (cons bundle-label-id (map find-label (:delivery-methods game))))]
        (print-flush " - Uploading to Trello... ")
        (if (and upload? (upload-to-trello! title md-description image-url trailer-url trello-labels))
          (println "OK")
          (do (println (if upload? "Failed" "Skipped"))
              (save-to-file! game md-description)))))))


(defn process-humble-url!
  [choice-month-url extract-data-fn upload?]
  (if (nil? choice-month-url)
    (println "Please supply a Humble choice month URL as parameter")
    (let [_ (println "Making request to" choice-month-url)
          html (web/load-hiccup choice-month-url)
          {:keys [games trello-label-name]} (extract-data-fn html)]
      (if (not (seq games))
        (println "No games found. Is the URL correct?")
        (do
          (println "Found" (count games) "games.")
          (let [bundle-label-id (and upload? (trello/create-label! board-id trello-label-name :red))]
            (process-games! games bundle-label-id upload?)))))))


(defn fetch-current-choice-bundle!
  "Scrapes the humble choice games for the current month and year"
  [upload-to-trello?]
  (let [now (LocalDateTime/now)
        month (-> now .getMonth .name .toLowerCase)
        year (-> now .getYear)]
    (process-humble-url!
      (str "https://www.humblebundle.com/subscription/" month "-" year)
      humble/extract-choice-data
      upload-to-trello?)
    (trello/sort-list! upload-list-id)))


(defn -main [& _args]
  (fetch-current-choice-bundle! true))


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


;; functions for REPL evaluation
(comment

  (process-humble-url! "https://www.humblebundle.com/games/sakura-series-bundle" humble/extract-bundle-data false)

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
  )
