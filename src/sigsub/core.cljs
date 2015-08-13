(ns sigsub.core
  (:require [sigsub.signal :as signal]
            [sigsub.reagent :as reagent]))

(defn make-base [parent]
      "Wraps an IWatchable and IDerefable (e.g. atom) object with a BaseSignal, which
      broadcasts when it is dereferenced and refreshes it's children when it
      changes (similar to reagent.ratom/RAtom)"
      (let [base (signal/BaseSignal. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (signal/-refresh-children base))))
           base))

(defn- nop-deactivate []
       (fn []))

(defn register-signal-skeleton
      "Registers a mapping from path to signal skeleton.
      A signal skeleton consists of a run-fn and a deactivate-fn which
      define the signals behavior.

      path is a vector of values e.g. [:page :form :field]
      When subscribing to or referencing a signal, any additional elements in
      the vector will be passed as an argument to run-fn and deactivate-fn on
      creation.
      e.g. [:page :form :field :name] will pass [:name] as the argument

      run-fn should be a higher order fn which is called on signal creation,
      with the path remainder as an argument, and returns a fn which will be
      called whenever a parent signal changes. The result of this function will
      represent the current value of the signal.

      deactivate-fn works similar to run-fn, except is called just before the
      signal is deactivated"
      ([path run-fn] (register-signal-skeleton path run-fn nop-deactivate))
      ([path run-fn deactivate-fn]
        (set! signal/registered-skeletons
              (assoc-in signal/registered-skeletons path
                        (signal/SignalSkeleton. run-fn deactivate-fn nil)))))

(defn register-default-signal-skeleton
      "Registers a signal skeleton that will be called when path does not
      refer to a registered signal skeleton. The entire path will be passed as
      an argument to the skeleton's functions"
      ([run-fn] (register-default-signal-skeleton run-fn nop-deactivate))
      ([run-fn deactivate-fn]
        (set! signal/default-skeleton
              (signal/SignalSkeleton. run-fn deactivate-fn nil))))

(defn get-in-atom-run-fn [atom]
      "Typical use case default run-fn. Takes an atom, wraps a base around it,
      then returns a higher order fn which will be close over the path on signal
      creation, and update the signal value by getting path in base whenever
      atom changes"
      (let [base (make-base atom)]
           (fn [path]
               (fn []
                   (get-in @base path)))))

(defn reference [path-fn]
      "Creates a reference to a signal.
      Always references an active signal.
      If a signal instance is deactivated and a signal instance representing the
      same path is activated elsewhere, a signal reference will reference the
      new signal. This is neccesary because these references are created to
      track dependent signals, which may deactivate and activate as different
      instances as needed."
      (signal/SignalReference. path-fn))

(defn subscribe-reagent [path-fn]
      "Creates a subscription that can bind to a reagent.ratom/Reaction.
      Paths are wrapped in fns so they can be dependent on other signals."
      (reagent/ReagentSubscription. path-fn nil nil {}))

(defn subscribe [path]
      "Creates a manual subscription to a signal. The signal is creating using
      the skeleton registered to path or the default skeleton.
      The signal will remain activated until this subscription is passed to
      unsubscribe."
      (let [sub (signal/ManualSubscription. (signal/path->signal path))]
           (signal/-subscribe sub)
           sub))

(defn unsubscribe [sub]
      "Unsubscribes a subscription which will deactivate the corresponding
      signal if no other subscriptions are made to it."
      (signal/-unsubscribe sub))

(defn query [path]
      "Performs a one time query to the signal represented by path."
      (let [sub (subscribe path)
            value @sub]
           (unsubscribe sub)
           value))

