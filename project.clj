(defproject hc-scraper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.7"]
                 [hickory "0.7.1"]]
  :main ^:skip-aot hc-scraper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
