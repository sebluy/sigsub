(defproject
  sigsub
  "0.1.0"
  :description "A signal network for supplying reagent views with data"
  :url "http://github.com/sebluy/sigsub"
  :license {:name "MIT"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [reagent "0.5.0"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.7"]]

  :clean-targets ^{:protect false} ["resources/public/js"]

  :profiles
  {:dev
   {:figwheel
    {:http-server-root "public"
     :nrepl-port       7888}
    :cljsbuild
    {:builds
     [{:id "test"
       :source-paths ["src" "test"]
       :figwheel     {:on-jsload "sigsub.core-test/run-all-tests"}
       :compiler     {:main                 "sigsub.core-test"
                      :source-map           true
                      :source-map-timestamp true
                      :optimizations        :none
                      :output-to            "resources/public/js/main.js"
                      :output-dir           "resources/public/js"
                      :asset-path           "js"}}]}}})
