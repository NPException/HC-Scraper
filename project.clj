(defproject hc-scraper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.json "1.0.0"]
                 [http-kit "2.4.0-alpha6"]
                 [hickory "0.7.1"]]
  :main ^:skip-aot hc-scraper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
