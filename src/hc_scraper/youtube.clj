(ns hc-scraper.youtube
  (:require [hc-scraper.web :refer [load-hiccup parse-html search-all search download]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.io File)))


(defn ^:private find-attribute
  [element tag search-attributes target-attribute]
  (-> element
      (search tag search-attributes)
      (nth 1)
      target-attribute))

(defn ^:private find-meta
  [video-page property]
  (find-attribute video-page :meta {:itemprop property} :content))

(defn ^:private thumbnail-url
  [video-page]
  (find-attribute video-page :meta {:property "og:image"} :content))

(defn ^:private find-channel-name
  [video-page]
  (-> video-page
      (search :span {:itemprop "author"})
      (find-attribute :link {:itemprop "name"} :content)))


(def ^:private initial-data-start "window[\"ytInitialData\"] =")
(def ^:private initial-data-end "}}};")

(defn ^:private find-data-script-tag
  [video-page]
  (->> (search-all video-page :script {})
       (filter (fn [tag]
                 (and (= (count tag) 3)
                      (string/includes? (nth tag 2) initial-data-start))))
       first))

(defn ^:private extract-initial-data-text
  [script-tag]
  (let [script-line (nth script-tag 2)
        start (+ (string/index-of script-line initial-data-start)
                 (count initial-data-start))]
    (subs script-line start)))

(defn ^:private extract-video-json-data
  [video-page]
  #_(spit "vid.edn" (with-out-str (clojure.pprint/pprint video-page)))
  (when-let [initial-data (some-> (find-data-script-tag video-page)
                                  (extract-initial-data-text)
                                  (json/read-str :key-fn keyword))]
    (reduce
      (fn [result partial-map] (reduce-kv assoc result partial-map))
      {}
      (get-in initial-data [:contents :twoColumnWatchNextResults :results :results :contents]))))


(defn ^:private extract-full-description
  [video-render-data]
  (->> video-render-data :videoSecondaryInfoRenderer :description :runs
       (map :text)
       string/join))


(defn ^:private extract-likes-and-dislikes
  [video-render-data]
  (let [like-dislike-tooltip (-> video-render-data :videoPrimaryInfoRenderer :sentimentBar :sentimentBarRenderer :tooltip)
        like-dislike-strings (string/split like-dislike-tooltip #" / ")]
    (->> like-dislike-strings
         (mapv #(string/replace % "." ""))                  ;; remove thousand-delimiters
         (mapv #(Long/parseLong %)))))


(defn ^:private fetch-data
  [video-url retries]
  (let [video-page (load-hiccup video-url)
        video-render-data (extract-video-json-data video-page)]
    (if (and (nil? video-render-data)
             (> retries 0))
      (recur video-url (dec retries))
      [video-page video-render-data])))


(defn load-video-data
  [relative-video-url]
  (let [end (or (string/index-of relative-video-url "&")
                (count relative-video-url))
        video-id (subs relative-video-url (count "/watch?v=") end)
        video-url (str "https://www.youtube.com" relative-video-url)
        [video-page video-render-data] (fetch-data video-url 4)
        [likes dislikes] (extract-likes-and-dislikes video-render-data)]
    {:id                video-id
     :name              (find-meta video-page "name")
     :channel           (find-channel-name video-page)
     :video-render-data video-render-data
     :cut-description   (find-meta video-page "description")
     :description       (extract-full-description video-render-data)
     :url               video-url
     :thumbnail-url     (thumbnail-url video-page)
     :views             (some-> (find-meta video-page "interactionCount")
                                Long/parseLong)
     :publish-date      (find-meta video-page "datePublished")
     :upload-date       (find-meta video-page "uploadDate")
     :playtime          (find-meta video-page "duration")
     :likes             likes
     :dislikes          dislikes
     :scrape-time       (java.util.Date.)}))


(defn store-video-data
  [relative-video-url base-dir channel-name store-thumbnail?]
  (let [data (load-video-data relative-video-url)
        file-id (str (:publish-date data) " " (:id data))]
    ;; small sanity check
    (when (and base-dir
               (-> base-dir File. .isDirectory)
               (or (nil? channel-name)
                   (= (:channel data) channel-name)))
      ;; create directories if necessary
      (let [thumbnails-dir (File. (str base-dir "/thumbnails/"))
            data-dir (File. (str base-dir "/data/"))]
        (when (and store-thumbnail? (.exists thumbnails-dir))
          (.mkdirs thumbnails-dir))
        (when-not (.exists data-dir)
          (.mkdirs data-dir)))
      ;; download thumbnail
      (when store-thumbnail?
        (if (:thumbnail-url data)
          (download (:thumbnail-url data) (str base-dir "/thumbnails/" file-id ".jpg"))
          (spit (str base-dir "/thumbnails/" file-id " missing.txt") "")))
      ;; store data as edn
      (let [file-path (str base-dir "/data/" file-id ".edn")]
        (spit file-path (with-out-str (clojure.pprint/pprint data)))
        (println "Created" file-path " - " (-> file-path io/as-file .length (quot 1000)) "kB")))))