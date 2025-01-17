(ns patreon.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hc-scraper.web :as web]
            [org.httpkit.client :as http])
  (:import (java.util Date)
           (java.util.concurrent LinkedBlockingQueue)))

(defonce rate-limit (atom 0))
(def ^Long rate-limit-sleep 200)

(defonce ^LinkedBlockingQueue request-queue (LinkedBlockingQueue.))
(defonce request-runner
         (future
           (while true
             (let [request-fn (.take request-queue)]
               (when (> @rate-limit 0)
                 (Thread/sleep rate-limit-sleep)
                 (swap! rate-limit dec))
               (request-fn)))))

(defn ^:private api-request
  [url cookies]
  (let [response (promise)
        make-request (fn execute-request []
                       (http/request
                         {:method  :get
                          :url     url
                          :headers {"Cookie" cookies}}
                         (fn [result]
                           (when (or (case (int (or (:status result) 0))
                                       (200 429) false
                                       true)
                                     (:error result))
                             (println (str "Patreon returned error for request to " url
                                           " -> status: " (prn-str (:status result)) ", error: " (prn-str (:error result)))))
                           ;; check for rate limiting
                           (if (= 429 (:status result))
                             (do (println (Date.) "Hit rate limit. Retry after short wait." url)
                                 (swap! rate-limit inc)
                                 (.put request-queue execute-request))
                             (deliver response result)))))]
    (.put request-queue make-request)
    (let [success? (= 200 (:status @response))]
      (when success?
        (json/read-str (:body @response) :key-fn keyword)))))

(defn prepare-media
  [{id :id {:keys [file_name download_url]} :attributes :as _media}]
  (let [qmark-index (str/last-index-of download_url "?")
        slash-index (str/last-index-of download_url "/" (or qmark-index (count download_url)))
        file_name (if (some? file_name)
                    file_name
                    (str id "_" (subs download_url (inc slash-index) (or qmark-index (count download_url)))))
        ; default to jpg if we get some weird borked filename
        file_name (if (str/starts-with? file_name "https://")
                    (str "UNKNOWN_" id ".jpg")
                    file_name)]
    {:id           id
     :file_name    file_name
     :download_url download_url}))

(defn media-map
  [edn-data]
  (->> (:included edn-data)
       (filter #(= (:type %) "media"))
       (keep prepare-media)
       (map (juxt :id identity))
       (into {})))


(defn extract-post
  [media-map {:keys [id attributes relationships] :as _post}]
  (let [title (:title attributes)
        date (or (:published_at attributes)
                 (:created_at attributes))
        media (->> relationships :media :data
                   (mapv #(media-map (:id %))))]
    {:title title
     :date  date
     :id    id
     :media media}))

(defn posts
  [edn-data media]
  (->> (:data edn-data)
       (filter #(= (:type %) "post"))
       (mapv #(extract-post media %))))

(defn next-page
  [edn-data]
  (->> edn-data :links :next))

(defn download-files
  [{:keys [title date media]} download-dir]
  (doseq [{:keys [file_name download_url]} media]
    (let [safe-title (str/replace (str (subs date 0 10) " - " title " - " file_name) #"[<>:\"/\\|?*]" "_")]
      (Thread/sleep 1000)
      (web/download download_url (io/file download-dir safe-title)))))

(defn fetch-data [path cookies download-dir]
  (when-let [edn-data (api-request path cookies)]
    (let [media (media-map edn-data)
          posts (posts edn-data media)]
      (doseq [post posts]
        (download-files post download-dir))
      (next-page edn-data))))

; get a start url by looking at user posts, then find a GET to "/api/posts" in the browser dev tools network tab
; put the request url and cookies string below, adjust output directory, then run the loop below

(def output-dir "")
(def cookies "")
(def start-url "")

(comment

  (loop [url start-url]
    (when url
      (println url)
      (recur (fetch-data url cookies output-dir))))

  ;
  )