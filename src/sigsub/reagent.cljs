(ns sigsub.reagent
  (:require [sigsub.signal :as signal]
            [reagent.impl.batching :as reagent-batching]
            [reagent.ratom :as reagent]))

(defonce reagent-pending #{})

(declare add-reagent-pending)
(declare remove-reagent-pending)

(deftype ReagentSubscription [path ^:mutable signal ^:mutable watches]
         IDeref
         (-deref [this]
                 (reagent/notify-deref-watcher! this)
                 (when (nil? signal)
                   (set! signal (signal/path->signal path)))
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
                           (signal/-add-child signal this))
                     (set! watches (assoc watches key f)))
         (-remove-watch [this key]
                        (set! watches (dissoc watches key))
                        (when (empty? watches)
                              (add-reagent-pending [signal this]))))

(defn- add-reagent-pending [sub]
       (set! reagent-pending (conj reagent-pending sub)))

(defn- reagent-batch-deactivate []
       (doseq [[signal sub] reagent-pending]
              (set! (.-signal sub) nil)
              (signal/-remove-child signal sub))
       (set! reagent-pending #{}))

(defn- reagent-deactivate-flush-loop []
       (reagent-batch-deactivate)
       (reagent-batching/do-after-flush reagent-deactivate-flush-loop))

(reagent-batching/do-after-flush reagent-deactivate-flush-loop)

