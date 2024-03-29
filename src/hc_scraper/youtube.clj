(ns hc-scraper.youtube
  (:require [hc-scraper.web :refer [load-url parse-html search-all search download]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.util Date)))


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
  (if-let [like-dislike-tooltip (-> video-render-data :videoPrimaryInfoRenderer :sentimentBar :sentimentBarRenderer :tooltip)]
    (->> (string/split like-dislike-tooltip #" / ")
         (mapv #(string/replace % "." ""))                  ;; remove thousand-delimiters
         (mapv #(Long/parseLong %)))
    [0 0]))


(defn ^:private fetch-data
  [video-url retries]
  (let [video-page-html (load-url video-url)
        video-page-hiccup (parse-html video-page-html)
        video-render-data (extract-video-json-data video-page-hiccup)]
    (if (and (nil? video-render-data)
             (> retries 0))
      (recur video-url (dec retries))
      [video-page-html video-page-hiccup video-render-data])))


(defn load-video-data
  "Loads and returns the video data, as well as the raw video page html"
  [relative-video-url]
  (let [end (or (string/index-of relative-video-url "&")
                (count relative-video-url))
        video-id (subs relative-video-url (count "/watch?v=") end)
        video-url (str "https://www.youtube.com" relative-video-url)
        [raw-html video-page video-render-data] (fetch-data video-url 4)
        [likes dislikes] (extract-likes-and-dislikes video-render-data)]
    [{:id                video-id
      :name              (find-meta video-page "name")
      :channel           (find-channel-name video-page)
      :video-render-data video-render-data
      :cut-description   (find-meta video-page "description")
      :description       (extract-full-description video-render-data)
      :url               video-url
      :thumbnail-url     (thumbnail-url video-page)
      ;; equal to views for videos. For live streams this represents some other value.
      :interaction-count (some-> (find-meta video-page "interactionCount")
                                 Long/parseLong)
      :views-text        (let [view-count-data (-> video-render-data :videoPrimaryInfoRenderer :viewCount :videoViewCountRenderer :viewCount)
                               video-view-count (:simpleText view-count-data)
                               stream-view-count (-> view-count-data :runs first :text)]
                           (or video-view-count stream-view-count))
      :publish-date      (find-meta video-page "datePublished")
      :upload-date       (find-meta video-page "uploadDate")
      :playtime          (find-meta video-page "duration")
      :likes             likes
      :dislikes          dislikes
      :scrape-time       (Date.)}
     raw-html]))


(defn store-video-data
  [relative-video-url base-dir channel-name store-thumbnail?]
  (let [[data raw-html] (load-video-data relative-video-url)
        file-id (str (:publish-date data) " " (:id data))]
    ;; small sanity check
    (when (and base-dir
               (or (-> base-dir io/file .isDirectory)
                   (-> base-dir io/file .mkdirs))
               (or (nil? channel-name)
                   (= (:channel data) channel-name)))
      ;; create directories if necessary
      (let [thumbnails-dir (File. (str base-dir "/thumbnails/"))
            raw-dir (File. (str base-dir "/raw/"))
            data-dir (File. (str base-dir "/data/"))]
        (when (and store-thumbnail? (not (.exists thumbnails-dir)))
          (.mkdirs thumbnails-dir))
        (when-not (.exists raw-dir)
          (.mkdirs raw-dir))
        (when-not (.exists data-dir)
          (.mkdirs data-dir)))
      ;; download thumbnail
      (when store-thumbnail?
        (if (:thumbnail-url data)
          (download (:thumbnail-url data) (str base-dir "/thumbnails/" file-id ".jpg"))
          (spit (str base-dir "/thumbnails/" file-id " missing.txt") "")))
      ;; store page html
      (let [file-path (str base-dir "/raw/" file-id ".html")]
        (spit file-path raw-html)
        (println "Created" file-path " - " (-> file-path io/file .length (quot 1000)) "kB"))
      ;; store data as edn
      (let [file-path (str base-dir "/data/" file-id ".edn")]
        (spit file-path (with-out-str (clojure.pprint/pprint data)))
        (println "Created" file-path " - " (-> file-path io/file .length (quot 1000)) "kB")))))



(comment
  ;; This is assuming you saved the source of a YT channel page to a file from your browser, where
  ;; you scrolled far enough to have all videos visible that you want to scrape data for.
  ;; Just making a request to the channel page won't work, because it doesn't load all the videos.
  (defonce channel-page (parse-html (slurp "some-channel-page-source.html")))
  (def channel-name "Mr Beast")

  ;; store data for all videos of a channel without thumbnails
  (let [base-dir (str "~/youtube-data/_VID_META_" (System/currentTimeMillis))]
    (-> base-dir java.io.File. .mkdirs)
    (time (->> (search-all channel-page :a {:id "video-title"})
               (pmap #(store-video-data (:href (nth % 1))
                                        base-dir
                                        channel-name
                                        false))
               dorun)))

  ;; store data for all videos one thread for each. don't over-use! might get rate limited.
  (let [base-dir (str "~/youtube-data/_VID_META_" (System/currentTimeMillis))]
    (-> base-dir java.io.File. .mkdirs)
    (time (->> (search-all channel-page :a {:id "video-title"})
               (mapv #(future (store-video-data (:href (nth % 1))
                                                base-dir
                                                channel-name
                                                false)))
               (map deref)
               dorun)))

  (defn relative-url-from-clipboard
    []
    (let [url (-> (java.awt.Toolkit/getDefaultToolkit)
                  (.getSystemClipboard)
                  (.getData java.awt.datatransfer.DataFlavor/stringFlavor))]
      ;; sanity check
      (java.net.URL. url)
      (subs url (string/last-index-of url "/"))))

  ;; load video data from url in clipboard and bind to var
  (def video-data (first (load-video-data (relative-url-from-clipboard))))

  ;; store video data from video url in clipboard
  (try
    (store-video-data (relative-url-from-clipboard)
                      "~/youtube-data/other"
                      nil
                      true)
    (catch Exception e
      (.printStackTrace e)))

  )