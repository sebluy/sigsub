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

(defn nop-deactivate []
      (fn []))

(defn register-signal-skeleton
      ([path run-fn] (register-signal-skeleton path run-fn nop-deactivate))
      ([path run-fn deactivate-fn]
        (set! signal/registered-skeletons
              (assoc-in signal/registered-skeletons path
                        (signal/SignalSkeleton. run-fn deactivate-fn nil)))))

(defn register-default-signal-skeleton
      ([run-fn] (register-default-signal-skeleton run-fn nop-deactivate))
      ([run-fn deactivate-fn]
        (set! signal/default-skeleton
              (signal/SignalSkeleton. run-fn deactivate-fn nil))))

(defn get-in-atom-run-fn [atom]
      (let [base (make-base atom)]
           (fn [path]
               (fn []
                   (get-in @base path)))))

(defn- reference [path-fn]
       (signal/SignalReference. path-fn))

(defn subscribe-reagent [path-fn]
       (reagent/ReagentSubscription. path-fn nil nil {}))

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

