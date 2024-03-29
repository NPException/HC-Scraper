(ns hc-scraper.markdown
  (:require [clojure.string :as string]))

(defn bold [x]
  (if (string/blank? x)
    x
    (str " **" (string/trim x) "** ")))

(defn italic [x]
  (if (string/blank? x)
    x
    (str " _" (string/trim x) "_ ")))

(def new-line "  \n")

(def new-section "\n\n")

(defn link [title url & [trello-option]]
  (let [title (if trello-option url title)]                 ; trello smartCard links only work if the title is the same as the url
    (str "[" title "]"
      "(" url
      (when trello-option
        (case trello-option
          :inline " \"smartCard-inline\""
          :block " \"smartCard-block\""
          :embed " \"smartCard-embed\""))
      ")")))

(defn image [alt url]
  (str "!" (link alt url)))

(defn code [x]
  (if (string/blank? x)
    x
    (str " `" (string/trim x) "` ")))

(defn code-section [x]
  (str "\n```\n" x "\n```\n"))

(defn ^:private li-element? [x]
  (and (vector? x)
       (= :li (first x))))

(defn ^:private source-element? [x]
  (and (vector? x)
       (= :source (first x))))

(defn heading [level x]
  (str "\n" (apply str (repeat level "#")) " " x "\n"))


(def ^:private known-entities
  {"&nbsp;" " "
   "&amp;" "&"
   "&quot;" "\""
   "&lt;" "<"
   "&gt;" ">"})

(def ^:private known-entities-regex
  (->> known-entities
       (map key)
       (string/join "|")
       re-pattern))

(defn as-markdown
  [element]
  (if (string? element)
    (-> (string/replace element known-entities-regex #(known-entities % %))
        (string/trim))
    (let [[tag attribs & content] element
          md-content #(apply str (map as-markdown content))]
      (cond
        (= tag :body) (md-content)
        (= tag :div) (md-content)
        (= tag :span) (md-content)
        (= tag :br) new-line
        (= tag :p) (str (md-content) new-section)
        (= tag :a) (if (:href attribs)
                     (link (md-content) (:href attribs))
                     (md-content))
        (= tag :img) (image (:alt attribs) (:src attribs))
        (= tag :strong) (apply str (map (comp bold as-markdown) content))
        (= tag :b) (apply str (map (comp bold as-markdown) content))
        (= tag :em) (apply str (map (comp italic as-markdown) content))
        (= tag :i) (apply str (map (comp italic as-markdown) content))
        (= tag :u) (apply str (map (comp italic as-markdown) content))
        (= tag :code) (apply str (map (comp code as-markdown) content))
        ;; TODO: support nested lists
        (= tag :ul) (->> content
                         (filter li-element?)
                         (map as-markdown)
                         (map #(str "- " %))
                         (string/join "\n")
                         (#(str new-section % new-section)))
        (= tag :li) (md-content)
        (= tag :video) (->> content
                            (filter source-element?)
                            first
                            second
                            :src
                            (image "VIDEO"))
        (re-matches #"h(\d{1,2})" (name tag)) (let [[_ level] (re-matches #"h(\d{1,2})" (name tag))]
                                                (heading (Integer/parseInt level) (md-content)))
        :unknown (code-section (pr-str element))))))
