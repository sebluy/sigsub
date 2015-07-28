(ns sigsub.core
  (:require [sigsub.manager :as manager]))

(defn get-signal [path]
      (let [signal (manager/path->signal path)]
           (manager/add-signal-dependency path signal)
           signal))

(defn dispose-signal [path]
      (manager/remove-signal-dependency path))

