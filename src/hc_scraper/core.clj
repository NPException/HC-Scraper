(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hc-scraper.trello :as trello]
            [hickory.core :as hickory]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.util Map]))


(defn ^:private submap?
  "Checks whether m contains all entries in sub."
  [^Map sub ^Map m]
  (.containsAll (.entrySet m) (.entrySet sub)))

(defn ^:private search-all
  "Searches the element hierarchy for elements with the given tag and matching attributes"
  [element target-tag target-attribs]
  (when (vector? element)
    (let [[tag attribs & children] element
          match? (and (or (nil? target-tag) (= tag target-tag))
                      (submap? target-attribs attribs))]
      (loop [remaining children
             results (if match? (cons element nil) '())]
        (if (empty? remaining)
          results
          (recur (rest remaining)
                 (concat results (search-all (first remaining) target-tag target-attribs))))))))


(defn ^:private search
  "Searches the element hierarchy for the first element with the given tag and matching attributes"
  [element target-tag target-attribs]
  (first (search-all element target-tag target-attribs)))


(defn ^:private parse-html [html]
  (->> html
       hickory/parse
       hickory/as-hiccup
       (filter vector?)
       first))

(defn ^:private slurp-hiccup [f]
  (-> f
      slurp
      parse-html))


(defn ^:private fetch-steam-url
  "Queries steamdb.info for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
  (let [clean-title (or (second (re-matches #"(.*?)( \+ \d+ DLCs?)$" title))
                        title)
        encoded-title (java.net.URLEncoder/encode ^String clean-title "UTF-8")
        html (slurp-hiccup (str "https://steamdb.info/search/?a=app&type=1&category=0&q=" encoded-title))
        candidates (search-all html :tr {:class "app"})
        match (->> candidates
                   (filter #(contains? (set %) [:td {} clean-title]))
                   first)
        [_ {:keys [data-appid]}] (or match (first candidates))]
    (when data-appid
      (str "https://store.steampowered.com/app/" data-appid))))

(defn print-flush [& args]
  (apply print args)
  (flush))


(defn ^:private process-game! [data trello-labels]
  (let [title (:title data)
        _ (println "Next:" title)
        genres (string/join ", " (:genres data))
        developers (string/join ", " (:developers data))
        yt-url (str "https://www.youtube.com/watch?v=" (-> data :carousel_content :youtube-link first))
        choice-url (:choice-url data)
        image-url (:image data)
        description (-> (:description data)
                        parse-html
                        (search :body {})
                        md/as-markdown)
        system-requirements (some-> (:system_requirements data)
                                    parse-html
                                    (search :body {})
                                    md/as-markdown)
        _ (print-flush " - Searching for Steam Page URL... ")
        steam-url (fetch-steam-url title)
        _ (println (if steam-url "OK" "Failed"))
        description-md (str
                         "Genres: " (md/italic genres) md/new-line
                         "Developers: " (md/bold developers)
                         md/new-section
                         (md/bold
                           (str (md/link "Steam Page" steam-url) " | " (md/link "Youtube Trailer" yt-url) " | " (md/link "Humble Choice Link" choice-url)))
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
    (if (trello/upload title description-md image-url yt-url trello-labels)
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
  (trello/create-label (string/capitalize delivery-method) "sky"))

(defn ^:private find-delivery-method-label
  [labels delivery-method]
  (if-let [[_ id] (first
                    (filter
                      #(-> (key %)
                           string/lower-case
                           (string/includes? delivery-method))
                      labels))]
    id
    (create-delivery-method-label delivery-method)))


(defn -main
  [& choice-month-urls]
  (if (empty? choice-month-urls)
    (println "Please supply at least one humble choice month URL as parameter")
    (doseq [choice-month-url choice-month-urls]
      (let [_ (println "Making request to" choice-month-url)
            html (slurp-hiccup choice-month-url)
            [_ _ json-data] (search html :script {:id "webpack-monthly-product-data"})
            raw-edn (-> json-data
                        (json/read-str :key-fn keyword))
            data (:contentChoiceOptions raw-edn)
            base-url (str "https://www.humblebundle.com/subscription/" (:productUrlPath data) "/")
            choice-month (:title data)
            games (-> data
                      :contentChoiceData
                      :initial
                      :content_choices)]
        (if (not games)
          (println "No Humble Choice games found. Is the URL correct?")
          (do
            (println "Found" (count games) "games.")
            (let [month-label-id (trello/create-label (str "HC " choice-month) "red")
                  all-labels (trello/all-labels)
                  find-label (memoize #(find-delivery-method-label all-labels %))]
              (doseq [game games]
                (let [game-url-name (name (key game))
                      game-data (assoc (val game)
                                  :game-url-name game-url-name
                                  :choice-url (str base-url game-url-name))
                      trello-labels (cons month-label-id (map find-label (:delivery_methods game-data)))]
                  (process-game! game-data trello-labels))))))))))
