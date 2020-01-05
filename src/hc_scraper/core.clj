(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hickory.core :as hickory]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.util Map]))


(defn submap?
  "Checks whether m contains all entries in sub."
  [^Map sub ^Map m]
  (.containsAll (.entrySet m) (.entrySet sub)))

(defn search
  "Searches the element hierarchy for an element with the given tag and matching attributes"
  [element target-tag target-attribs]
  (when (vector? element)
    (let [[tag attribs & children] element]
      (if (and (or (nil? target-tag) (= tag target-tag))
               (submap? target-attribs attribs))
        element
        (loop [remaining children]
          (if-let [found (search (first remaining) target-tag target-attribs)]
            found
            (when remaining
              (recur (next remaining)))))))))


(defn parse-html [html]
  (->> html
       hickory/parse
       hickory/as-hiccup
       (filter vector?)
       first))

(defn slurp-hiccup [f]
  (-> f
      slurp
      parse-html))


(defn fetch-steam-url
  "Queries steamdb.info for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (let [[_ clean-title] (re-matches #"(.*?)( \+ \d+ DLCs?)$" title)
        encoded-title (java.net.URLEncoder/encode ^String (or clean-title title) "UTF-8")
        html (slurp-hiccup (str "https://steamdb.info/search/?a=app&type=1&category=0&q=" encoded-title))
        ;; TODO: check results for exact title match instead of grabbing the first row.
        ;; Example: https://steamdb.info/search/?a=app&q=Street+Fighter+V
        ;; only fallback to first row if no exact match is found
        [_ {:keys [data-appid]}] (search html :tr {:class "app"})]
    (when data-appid
      (str "https://store.steampowered.com/app/" data-appid))))


(defn parse-game [data]
  (let [title (:title data)
        genres (string/join ", " (:genres data))
        developers (string/join ", " (:developers data))
        steam-url (fetch-steam-url title)
        yt-url (str "https://www.youtube.com/watch?v=" (-> data :carousel_content :youtube-link first))
        choice-url (:choice-url data)
        image-url (:image data)
        description (-> (parse-html (:description data))
                        (search :body {})
                        md/as-markdown)]
    (str
      (md/heading 1 title) "\n"
      (md/image image-url) md/new-line
      "Genres: " (md/italic genres) md/new-line
      "Developers: " (md/bold developers)
      md/new-section
      (md/bold
        (str (md/link "Steam Page" steam-url) " | " (md/link "Youtube Trailer" yt-url) " | " (md/link "Humble Choice Link" choice-url)))
      md/new-section
      "-----"
      md/new-section
      description
      ;; TODO: add system specs
      )))


(defn -main
  [& [choice-month-url]]
  (if (not choice-month-url)
    (println "Please supply a humble choice month URL as parameter")
    (let [html (slurp-hiccup choice-month-url)
          [_ _ json-data] (search html :script {:id "webpack-monthly-product-data"})
          raw-edn (-> json-data
                      (json/read-str :key-fn keyword))
          data (:contentChoiceOptions raw-edn)
          base-url (str "https://www.humblebundle.com/subscription/" (:productUrlPath data) "/")
          games (-> data
                    :contentChoiceData
                    :initial
                    :content_choices)]
      (doseq [game games]
        (let [game-url-name (name (key game))
              game-data (assoc (val game) :choice-url (str base-url game-url-name))
              game-markdown (parse-game game-data)
              file (str "./games/" game-url-name ".md")]
          ;; TODO: upload to Trello board
          (io/make-parents file)
          (spit file game-markdown))))))

