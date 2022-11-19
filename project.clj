(defproject hc-scraper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/tools.cli "1.0.206"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.1"]
                 [http-kit "2.6.0"]
                 [hickory "0.7.1"]]
  :main ^:skip-aot hc-scraper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
