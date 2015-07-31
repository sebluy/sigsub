(ns sigsub.core
  (:require [clojure.set :as set]
            [reagent.ratom :as reagent]))

(defonce capturing? false)
(defonce captured-parents nil)
(defonce signal-being-captured nil)
(defonce refresh-queue (sorted-map))

(declare path->signal)

(declare enqueue-refresh)
(declare refresh)

(declare run-with-deref-capture)
(declare notify-deref-watcher)
(declare bind-dependency)
(declare unbind-dependency)

(declare activate-signal)
(declare deactivate-signal)

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

(defprotocol IDisposable
             (-dispose [this]))

(defprotocol ISubscribable
             (-subscribe [this])
             (-unsubscribe [this]))

(defprotocol IDepth
             (-get-depth [this]))

(deftype DerivedSignal [path f
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
               (let [value (f)]
                    (when-not (= current-value value)
                              (set! current-value value)
                              (enqueue-refresh children))))
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child))
                        (if (= (count children) 0)
                          (-dispose this)))
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
         IDisposable
         (-dispose [this]
                   (-update-parents this #{})
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

(deftype SignalReference [path]
         IDeref
         (-deref [this]
                 @(path->signal path)))

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

(deftype ReagentSubscription [signal ^:mutable watches]
         IDeref
         (-deref [this]
                 (reagent/notify-deref-watcher! this)
                 @signal)
         IRefreshable
         (-refresh [this]
                   (let [current-value @signal]
                        (-notify-watches this
                                         (not current-value) current-value)))
         IDepth
         (-get-depth [this] (inc (-get-depth signal)))
         IWatchable
         (-notify-watches [this oldval newval]
                          (doseq [[key f] watches]
                                 (f key this oldval newval)))
         (-add-watch [this key f]
                     (when (empty? watches)
                           (-add-child signal this))
                     (set! watches (assoc watches key f)))
         (-remove-watch [this key]
                        (set! watches (dissoc watches key))
                        (when (empty? watches)
                              (-remove-child signal this))))



(defn- make-derived [path f]
      (let [derived (DerivedSignal. path f nil -1 #{} #{})]
           (-refresh derived)
           (activate-signal path derived)
           derived))

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

; subscriptions

(defonce active-signals {})
(defonce default-signal-fn nil)
(defonce registered-derived-signal-fns {})

(defn- path->derived-signal-fn [path]
       (loop [node registered-derived-signal-fns path path]
             (cond (map? node)
                   (recur (node (first path)) (rest path))
                   (ifn? node)
                   (node path))))

(defn- path->signal-fn [path]
      (if-let [f (path->derived-signal-fn path)]
              f
              #(default-signal-fn path)))

(defn- path->signal [path]
      (if-let [signal (active-signals path)]
              signal
              (make-derived path (path->signal-fn path))))

(defn- activate-signal [path signal]
      (set! active-signals (assoc active-signals path signal)))

(defn- deactivate-signal [path]
      (set! active-signals (dissoc active-signals path)))

(defn- reference [path]
      (SignalReference. path))

(defn- reagent-subscribe [path]
       (ReagentSubscription. (path->signal path) {}))

(defn make-base [parent]
      (let [base (BaseSignal. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (-refresh-children base))))
           base))

(defn register-derived-signal-fn [path f]
      (set! registered-derived-signal-fns
            (assoc-in registered-derived-signal-fns path f)))

(defn register-default-signal-fn [f]
      (set! default-signal-fn f))

(defn get-in-atom-signal-fn [atom]
      (let [base (make-base atom)]
           (fn [path]
               (get-in @base path))))

(defn subscribe [path]
      (let [sub (ManualSubscription. (path->signal path))]
           (-subscribe sub)
           sub))

(defn unsubscribe [sub]
      (-unsubscribe sub))

(defn query [path]
      (let [sub (subscribe path)
            value @sub]
           (unsubscribe sub)
           value))

