(ns sigsub.signal
  (:require [clojure.set :as set]))

(defonce capturing? false)
(defonce captured-parents nil)
(defonce signal-being-captured nil)
(defonce refresh-queue (sorted-map))

(defonce active-signals {})
(defonce default-skeleton nil)
(defonce registered-skeletons {})

(declare path->signal)

(declare enqueue-refresh)
(declare refresh)

(declare run-with-deref-capture)
(declare notify-deref-watcher)
(declare bind-dependency)
(declare unbind-dependency)

(declare activate-signal)
(declare deactivate-signal)

(defrecord SignalSkeleton [run-fn deactivate-fn args])

(defprotocol IParent
             (-add-child [this child])
             (-remove-child [this child]))

(defprotocol IChild
             (-update-parents [this parents])
             (-add-parent [this parent])
             (-remove-parent [this parent]))

(defprotocol ISeed
             (-refresh-children [this]))

(defprotocol IRunnable
             (-run [this]))

(defprotocol IRefreshable
             (-refresh [this]))

(defprotocol ISubscribable
             (-subscribe [this])
             (-unsubscribe [this]))

(defprotocol IDepth
             (-get-depth [this]))

(defprotocol IDeactivatable
             (-deactivate [this]))

; differences between reagent.ratom/Reaction
; - breadth first signal propagation (instead of depth first)
;     to avoid redundant recomputations
; - recomputes on = (instead of identical?)
(deftype DerivedSignal [path run-fn deactivate-fn
                        ^:mutable current-value
                        ^:mutable depth
                        ^:mutable parents ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 current-value)
         IRefreshable
         (-refresh [this]
                   (let [new-parents (run-with-deref-capture this)]
                        (-update-parents this new-parents)))
         IRunnable
         (-run [this]
               (let [value (run-fn)]
                    (when-not (= current-value value)
                              (set! current-value value)
                              (enqueue-refresh children))))
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child))
                        (if (= (count children) 0)
                          (-deactivate this)))
         IDepth
         (-get-depth [this]
                     depth)
         IChild
         (-update-parents [this new-parents]
                          (let [to-remove (set/difference parents new-parents)
                                to-add (set/difference new-parents parents)]
                               (doseq [retired-parent to-remove]
                                      (unbind-dependency retired-parent this))
                               (doseq [new-parent to-add]
                                      (bind-dependency new-parent this))))
         (-add-parent [this parent]
                      (let [parent-depth (-get-depth parent)]
                           (if (> parent-depth depth)
                             (set! depth (inc parent-depth))))
                      (set! parents (conj parents parent)))
         (-remove-parent [this parent]
                         (set! parents (disj parents parent))
                         (let [parent-depth (-get-depth parent)]
                              (if (= (inc parent-depth) depth)
                                (set! depth (->> (map -get-depth parents)
                                                 (apply max)
                                                 (inc))))))
         IDeactivatable
         (-deactivate [this]
                      (-update-parents this #{})
                      (deactivate-fn))
                      (deactivate-signal path)
                      (set! parents nil)
                      (set! children nil)
                      (set! current-value nil)))

(deftype BaseSignal [parent ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 @parent)
         IDepth
         (-get-depth [this] 0)
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child)))
         ISeed
         (-refresh-children [this]
                            (enqueue-refresh children)
                            (refresh)))

(deftype SignalReference [path-fn]
         IDeref
         (-deref [this]
                 @(path->signal (path-fn))))

(deftype ManualSubscription [signal]
         IDeref
         (-deref [this]
                 @signal)
         IRefreshable
         (-refresh [this])
         IDepth
         (-get-depth [this] (inc (-get-depth signal)))
         ISubscribable
         (-subscribe [this]
                     (-add-child signal this))
         (-unsubscribe [this]
                       (-remove-child signal this)))

(defn- add-refresh-queue [signal depth]
       (if-let [depth-queue (refresh-queue depth)]
               (set! refresh-queue
                     (assoc refresh-queue depth (conj depth-queue signal)))
               (set! refresh-queue
                     (assoc refresh-queue depth #{signal}))))

(defn- remove-refresh-queue [signal depth]
       (let [depth-queue (refresh-queue depth)]
            (if (= 1 (count depth-queue))
              (set! refresh-queue (dissoc refresh-queue depth))
              (set! refresh-queue
                    (assoc refresh-queue depth (disj depth-queue signal))))))

(defn- enqueue-refresh [signals]
       (doseq [signal signals]
              (add-refresh-queue signal (-get-depth signal))))

(defn- refresh []
       (while (seq refresh-queue)
              (doseq [depth-queue (vals refresh-queue)]
                     (doseq [signal depth-queue]
                            (remove-refresh-queue signal (-get-depth signal))
                            (-refresh signal)))))

(defn- run-with-deref-capture [signal]
       (if capturing?
         (let [child signal-being-captured
               childs-captured captured-parents]
              (set! captured-parents #{})
              (set! signal-being-captured signal)
              (-run signal)
              (set! signal-being-captured child)
              (let [captured captured-parents]
                   (set! captured-parents childs-captured)
                   captured))
         (do (set! captured-parents #{})
             (set! signal-being-captured signal)
             (set! capturing? true)
             (-run signal)
             (set! capturing? false)
             (set! signal-being-captured nil)
             (let [captured captured-parents]
                  (set! captured-parents nil)
                  captured))))

(defn- unbind-dependency [parent child]
       (-remove-parent child parent)
       (-remove-child parent child))

(defn- bind-dependency [parent child]
       (-add-parent child parent)
       (-add-child parent child))

(defn- notify-deref-watcher [signal]
       (when capturing?
             (set! captured-parents (conj captured-parents signal))))

(defn- path->registered-skeleton [path]
       (loop [node registered-skeletons path path]
             (cond (= (type node) `~SignalSkeleton)
                   (assoc node :args path)
                   (map? node)
                   (recur (node (first path)) (rest path)))))

(defn- path->skeleton [path]
       (if-let [skeleton (path->registered-skeleton path)]
               skeleton
               (assoc default-skeleton :args path)))

(defn- roust-skeleton [path]
       (let [skeleton (path->skeleton path)
             run-fn ((:run-fn skeleton) (:args skeleton))
             deactivate-fn ((:deactivate-fn skeleton) (:args skeleton))
             derived (DerivedSignal. path run-fn deactivate-fn nil -1 #{} #{})]
            (-refresh derived)
            (activate-signal path derived)
            derived))

(defn- path->signal [path]
       (if-let [signal (active-signals path)]
               signal
               (roust-skeleton path)))

(defn- activate-signal [path signal]
       (set! active-signals (assoc active-signals path signal)))

(defn- deactivate-signal [path]
       (set! active-signals (dissoc active-signals path)))

