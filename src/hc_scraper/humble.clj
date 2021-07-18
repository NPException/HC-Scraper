(ns hc-scraper.humble
  (:require [hc-scraper.web :as web]
            [clojure.data.json :as json]))

;; game data shape
(comment
  {
   :title "Game Title"
   :game-url-name "game-title"                              ;; url- and filesystem-safe name
   :genres #_optional ["Genre 1" "Genre 2"]
   :developers #_optional ["dev studio A" "dev studio B"]
   :delivery-methods ["steam" "gog" "epic"]                 ;; on which store the game is delivered
   :trailer-url #_optional ["https://www.youtube.com/watch?v=abcd1234"]
   :bundle-url "https://url-to-the-bundle-or-the-specific-game-page-within-the-bundle"
   :image-url "https://link-to-title-card-image.jpg"
   :description-html "<b>Description of the game in <i>HTML</i> format</b>"
   :system-requirements-html #_optional "Requirements section in <i>HTML</i> format"
   }
  )

(defn build-choice-game-data
  ;; input in this case is a map entry from the games map
  [base-url [key data]]
  {:title (:title data)
   :game-url-name (name key)
   :genres (:genres data)
   :developers (:developers data)
   :delivery-methods (:delivery_methods data)
   :trailer-url (some->> data :carousel_content :youtube-link first
                         (str "https://www.youtube.com/watch?v="))
   :bundle-url (str base-url (name key))
   :image-url (:image data)
   :description-html (:description data)
   :system-requirements-html (:system_requirements data)})


(defn ^:private one-of
  [m & keys]
  (when keys
    (or (get m (first keys))
        (recur m (next keys)))))


(defn extract-choice-data
  [page-hiccup]
  (let [[_ _ json-data] (web/search page-hiccup :script {:id "webpack-monthly-product-data"} true)
        raw-edn (json/read-str json-data :key-fn keyword)
        data (:contentChoiceOptions raw-edn)
        base-url (str "https://www.humblebundle.com/subscription/" (:productUrlPath data) "/")
        choice-month (:title data)
        games-map (-> data
                      :contentChoiceData
                      (one-of :initial :initial-without-order)
                      :content_choices)]
    {:trello-label-name (str "HC " choice-month)
     :games (mapv #(build-choice-game-data base-url %) games-map)}))
