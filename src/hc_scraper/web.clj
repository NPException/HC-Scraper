(ns hc-scraper.web
  (:require [hickory.core :as hickory]
            [clojure.java.io :as io]
            [org.httpkit.sni-client :as sni-client]
            [org.httpkit.client :as http])
  (:import (java.util Map)
           (java.io Closeable)))

(alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client))

(def tag
  "returns the tag keyword of the hiccup element"
  first)

(def attribs
  "returns the attribute map of the hiccup element"
  second)

(def body
  "returns the children/body of the hiccup element"
  nnext)


(defn ^:private submap?
  "Checks whether m contains all entries in sub."
  [^Map sub ^Map m case-sensitive?]
  (loop [ks (keys sub)]
    (or (nil? ks)
        (let [a (get sub (first ks) ::NOT_FOUND)
              b (get m (first ks) ::NOT_FOUND)]
          (if (or (= a b)
                  (and (not case-sensitive?)
                       (string? a)
                       (string? b)
                       (= (.toLowerCase ^String a) (.toLowerCase ^String b))))
            (recur (next ks))
            false)))))


(defn search-all
  "Searches a hiccup element hierarchy for elements with the given tag and matching attributes"
  [element target-tag target-attribs & [case-sensitive?]]
  (when (vector? element)
    (let [match? (and (or (nil? target-tag) (= (tag element) target-tag))
                      (submap? target-attribs (attribs element) case-sensitive?))]
      (loop [children (body element)
             results (if match? (cons element nil) '())]
        (if (empty? children)
          results
          (recur (rest children)
                 (concat results (search-all (first children) target-tag target-attribs case-sensitive?))))))))


(defn search
  "Searches a hiccup element hierarchy for the first element with the given tag and matching attributes"
  [element target-tag target-attribs & [case-sensitive?]]
  (first (search-all element target-tag target-attribs case-sensitive?)))


(defn parse-html
  "Parses the given HTML string to a hiccup data structure."
  [html]
  (->> html
       hickory/parse
       hickory/as-hiccup
       (filter vector?)
       first))


(defn load-html [url]
  (some->
    (http/request {:method :get :url url})
    deref
    :body))


(defn load-hiccup [url]
  (some->
    url
    load-html
    parse-html))


(defn download
  "Downloads data from the given URL, and outputs it to f via spit."
  [url f]
  ;; TODO try to hack around http-kit to make this not load the whole file into memory first
  (println "Start downloading" f " - "
           (-> @(http/request {:method :head :url url}) :headers :content-length
               Long/parseLong
               (quot 1000))
           "kB")
  (with-open [^Closeable in (:body @(http/request {:url url :as :stream}))
              out (io/output-stream f)]
    (io/copy in out))
  (println "Finished downloading" f)
  (flush)
  true)
