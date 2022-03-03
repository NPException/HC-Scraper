(ns hc-scraper.humble
  (:require [clojure.data.json :as json]
            [hc-scraper.web :as web]))

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

(defn ^:private build-choice-game-data
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
  [m & key-fns]
  (when key-fns
    (or ((first key-fns) m)
        (recur m (next key-fns)))))


(defn extract-choice-data
  [page-hiccup]
  (let [[_ _ json-data] (web/search page-hiccup :script {:id "webpack-monthly-product-data"} true)
        raw-edn (json/read-str json-data :key-fn keyword)
        data (:contentChoiceOptions raw-edn)
        base-url (str "https://www.humblebundle.com/subscription/" (:productUrlPath data) "/")
        choice-month (:title data)
        games-map (-> data
                      :contentChoiceData
                      (one-of :initial :initial-without-order :game_data)
                      (one-of :content_choices identity))]
    {:trello-label-name (str "HC " choice-month)
     :games (mapv #(build-choice-game-data base-url %) games-map)}))


(defn ^:private extract-bundle-game-delivery-methods
  [data]
  (let [availability-data (:availability_icons data)
        names-map (:human_names availability-data)
        delivery-keys (keys (:delivery_to_platform availability-data))]
    (mapv names-map delivery-keys)))

(defn ^:private build-bundle-game-data
  ;; input in this case is a map entry from the games map
  [base-url logo-url bundle-name [_key data]]
  {:title (:human_name data)
   :game-url-name (:machine_name data)
   ;; TODO: check future bundles for genre
   :genres nil
   :developers (mapv :developer-name (:developers data))
   :delivery-methods (extract-bundle-game-delivery-methods data)
   :trailer-url (some->> data :youtube_link
                         (str "https://www.youtube.com/watch?v="))
   :bundle-url (str base-url (:machine_name data))
   :image-url (-> data :resolved_paths :featured_image)
   :description-html (str
                       (when logo-url
                         (str "<img src=\"" logo-url "\" alt=\"" bundle-name "\"><br>"))
                       (:description_text data))
   ;; TODO: check future bundles for sys-requirements
   :system-requirements-html nil})


(defn extract-bundle-data
  [page-hiccup]
  (let [[_ _ json-data] (web/search page-hiccup :script {:id "webpack-bundle-page-data"} true)
        raw-edn (json/read-str json-data :key-fn keyword)
        bundle-data (:bundleData raw-edn)
        bundle-name (-> bundle-data :basic_data :human_name)
        base-url (str "https://www.humblebundle.com/" (:page_url bundle-data) "/")
        logo-url (-> bundle-data :basic_data :logo)
        games-map (:tier_item_data bundle-data)]
    {:trello-label-name bundle-name
     :games (mapv #(build-bundle-game-data base-url logo-url bundle-name %) games-map)}))
