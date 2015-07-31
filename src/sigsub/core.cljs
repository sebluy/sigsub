(ns sigsub.core
  (:require [sigsub.signal :as signal]
            [sigsub.reagent :as reagent]))

(defn make-base [parent]
      (let [base (signal/BaseSignal. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (signal/-refresh-children base))))
           base))


(defn register-derived-signal-fn [path f]
      (set! signal/registered-derived-signal-fns
            (assoc-in signal/registered-derived-signal-fns path f)))

(defn register-default-signal-fn [f]
      (set! signal/default-signal-fn f))

(defn get-in-atom-signal-fn [atom]
      (let [base (make-base atom)]
           (fn [path]
               (get-in @base path))))

(defn- reference [path]
       (signal/SignalReference. path))

(defn subscribe-reagent [path]
       (reagent/ReagentSubscription. (signal/path->signal path) {}))

(defn subscribe [path]
      (let [sub (signal/ManualSubscription. (signal/path->signal path))]
           (signal/-subscribe sub)
           sub))

(defn unsubscribe [sub]
      (signal/-unsubscribe sub))

(defn query [path]
      (let [sub (subscribe path)
            value @sub]
           (unsubscribe sub)
           value))

