(ns sigsub.reagent
  (:require [sigsub.signal :as signal]
            [reagent.impl.batching :as reagent-batching]
            [reagent.ratom :as reagent]))

(defonce reagent-pending #{})

(declare add-reagent-pending)
(declare remove-reagent-pending)

(deftype ReagentSubscription [signal ^:mutable watches]
         IDeref
         (-deref [this]
                 (reagent/notify-deref-watcher! this)
                 @signal)
         signal/IRefreshable
         (-refresh [this]
                   (let [current-value @signal]
                        (-notify-watches this
                                         (not current-value) current-value)))
         signal/IDepth
         (-get-depth [this] (inc (signal/-get-depth signal)))
         IWatchable
         (-notify-watches [this oldval newval]
                          (doseq [[key f] watches]
                                 (f key this oldval newval)))
         (-add-watch [this key f]
                     (when (empty? watches)
                           (remove-reagent-pending this)
                           (signal/-add-child signal this))
                     (set! watches (assoc watches key f)))
         (-remove-watch [this key]
                        (set! watches (dissoc watches key))
                        (when (empty? watches)
                              (add-reagent-pending this))))

(defn- add-reagent-pending [sub]
      (set! reagent-pending (conj reagent-pending sub)))

(defn- remove-reagent-pending [sub]
      (set! reagent-pending (disj reagent-pending sub)))

(defn- reagent-batch-deactivate []
      (doseq [sub reagent-pending]
             (signal/-remove-child (.-signal sub) sub))
      (set! reagent-pending #{}))

(defn- reagent-deactivate-flush-loop []
      (reagent-batch-deactivate)
      (reagent-batching/do-after-flush reagent-deactivate-flush-loop))

(reagent-batching/do-after-flush reagent-deactivate-flush-loop)

