(ns ^:figwheel-no-load dev.env
  (:require [figwheel.client :as figwheel]
            [sigsub.core]
            [test.sigsub.core]))

(enable-console-print!)

(figwheel/watch-and-reload
  :websocket-url "ws://localhost:3449/figwheel-ws")
