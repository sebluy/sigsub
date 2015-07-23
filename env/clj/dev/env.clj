(ns dev.env
  (:require [figwheel-sidecar.repl-api :as repl]))

(defn browser-repl []
  (repl/cljs-repl))

(browser-repl)
