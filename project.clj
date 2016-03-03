(defproject chess "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler chess.core/app
         :nrepl {:start? true}}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.4.0"]])
