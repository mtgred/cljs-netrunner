(defproject netrunner "1.0"
  ; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.zeromq/jeromq "0.3.4"]
                 [cheshire "5.4.0"]
                 [org.omcljs/om "0.8.8"]
                 [sablono "0.3.4"]
                 [environ "1.0.0"]
                 [com.novemberain/monger "3.0.0-rc2"]
                 [org.slf4j/slf4j-nop "1.7.12"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [differ "0.2.1"]]

  :profiles {:dev {:dependencies [[figwheel "0.5.2"]
                                  [figwheel-sidecar "0.5.0-6"]
                                  [com.cemerick/piggieback "0.2.1"]]}}

  ; aot only the namespaces needed for the main game in uberjar
  :aot [game.utils
        game.main
        game.macros
        game.core]
  :main game.main

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.2"]
            [com.gfredericks/lein-sha-version "0.1.1-p1"]]

  :source-paths ["src/clj" "src/cljs"]

  :jar-name "netrunner.jar"
  :uberjar-name "netrunner-standalone.jar"

  :omit-source true

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs"]
       :compiler {:output-to "resources/public/cljs/app.js"
                  :output-dir "resources/public/cljs"
                  :optimizations :none
                  :source-map true}}
      {:id "prod"
       :source-paths ["src/cljs/netrunner"]
       :compiler {:output-to "resources/public/js/app.js"
                  :output-dir "out"
                  :optimizations :advanced
                  :pretty-print false
                  :closure-warnings {:externs-validation :off}
                  :externs ["resources/public/lib/jquery/jquery.min.js"
                            "resources/public/lib/jqueryui/jquery-ui.min.js"
                            "resources/public/lib/moment/min/moment.min.js"
                            "resources/public/lib/bootstrap/dist/js/bootstrap.min.js"
                            "node_modules/socket.io-client/socket.io.js"]}}]}

  :figwheel {:http-server-root "public"
             :server-port 3449
             :css-dirs ["resources/public/css"]})
