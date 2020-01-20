(ns hc-scraper.html
  (:require [hickory.core :as hickory])
  (:import [java.util Map]))


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
  [^Map sub ^Map m]
  (.containsAll (.entrySet m) (.entrySet sub)))

(defn search-all
  "Searches a hiccup element hierarchy for elements with the given tag and matching attributes"
  [element target-tag target-attribs]
  (when (vector? element)
    (let [match? (and (or (nil? target-tag) (= (tag element) target-tag))
                      (submap? target-attribs (attribs element)))]
      (loop [children (body element)
             results (if match? (cons element nil) '())]
        (if (empty? children)
          results
          (recur (rest children)
                 (concat results (search-all (first children) target-tag target-attribs))))))))


(defn search
  "Searches a hiccup element hierarchy for the first element with the given tag and matching attributes"
  [element target-tag target-attribs]
  (first (search-all element target-tag target-attribs)))


(defn parse-html
  "Parses the given HTML string to a hiccup data structure."
  [html]
  (->> html
       hickory/parse
       hickory/as-hiccup
       (filter vector?)
       first))


(defn slurp-hiccup [f]
  (-> f
      slurp
      parse-html))
