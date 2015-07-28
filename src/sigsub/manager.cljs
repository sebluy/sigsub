(ns sigsub.manager
  (:require [sigsub.signal :as signal])
  (:refer-clojure :exclude [swap!]))

(def current-signals {})
(def current-signal-counts {})
(def default-signal-fn nil)
(def registered-derived-signal-fns {})

(defn register-derived-signal-fn [path f]
      (set! registered-derived-signal-fns
            (assoc-in registered-derived-signal-fns path f)))

(defn register-default-signal-fn [f]
      (set! default-signal-fn f))

(defn get-in-atom-signal-fn [atom]
      (let [base (signal/make-base atom)]
           (fn [path]
               (get-in @base path))))

(defn- path->derived-signal-fn [path]
       (loop [node registered-derived-signal-fns path path]
             (cond (map? node)
                   (recur (node (first path)) (rest path))
                   (ifn? node)
                   (node path))))

(defn path->signal-fn [path]
      (if-let [f (path->derived-signal-fn path)]
              f
              #(default-signal-fn path)))

(defn path->signal [path]
      (if-let [signal (current-signals path)]
              signal
              (signal/make-derived path (path->signal-fn path))))

(defn add-signal-dependency [path signal]
      (let [count (current-signal-counts path)]
           (if (nil? count)
             (do (set! current-signals (assoc current-signals path signal))
                 (set! current-signal-counts (assoc current-signal-counts path 1)))
             (set! current-signal-counts (assoc current-signal-counts path (inc count))))))

(defn remove-signal-dependency [path]
      (let [count (current-signal-counts path)]
           (if (= count 1)
             (do (set! current-signals (dissoc current-signals path))
                 (set! current-signal-counts (dissoc current-signal-counts path)))
             (set! current-signal-counts (assoc current-signal-counts path (dec count))))))



