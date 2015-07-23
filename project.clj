(defproject
  sigsub
  "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.7"]]

  :hooks [leiningen.cljsbuild]

  :figwheel {:http-server-root "public"
             :nrepl-port       7888}

  :clean-targets ^{:protect false} ["resources/public/js"]

  :cljsbuild
  {:builds
   {:dev
    {:source-paths ["src" "test"]
     :figwheel      true
     :compiler     {:output-to     "resources/public/js/main.js"
                    :output-dir    "resources/public/js/out"
                    :asset-path    "js/out"
                    :source-map    true
                    :optimizations :none
                    :pretty-print  true}}}})
