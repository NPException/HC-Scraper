(ns hc-scraper.humble-choice)

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
   }
  )

(defn extract-game-data
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
