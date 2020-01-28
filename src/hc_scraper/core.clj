(ns hc-scraper.core
  (:gen-class)
  (:require [hc-scraper.markdown :as md]
            [hc-scraper.trello :as trello]
            [hc-scraper.web :refer [load-hiccup parse-html search-all search]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]))


(defn ^:private fetch-steam-url
  "Queries steamdb.info for the game title, and returns the first store page link found.
  If the game comes with DLCs the title is cleaned up if possible."
  [title]
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
    (when data-appid
      (md/link
        (str "Steam Page" (when-not match " (?)"))
        (str "https://store.steampowered.com/app/" data-appid)))))

(defn ^:private print-flush [& args]
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
                        (search :body nil)
                        md/as-markdown)
        system-requirements (some-> (:system_requirements data)
                                    parse-html
                                    (search :body nil)
                                    md/as-markdown)
        _ (print-flush " - Searching for Steam Page URL... ")
        md-steam-link (fetch-steam-url title)
        _ (println (if md-steam-link "OK" "Failed"))
        description-md (str
                         "Genres: " (md/italic genres) md/new-line
                         "Developers: " (md/bold developers)
                         md/new-section
                         (md/bold
                           (str md-steam-link " | " (md/link "Youtube Trailer" yt-url) " | " (md/link "Humble Choice Link" choice-url)))
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
            html (load-hiccup choice-month-url)
            [_ _ json-data] (search html :script {:id "webpack-monthly-product-data"} true)
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
