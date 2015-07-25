(ns sigsub.signal)

(def capturing? false)
(def signal-being-captured (atom nil))
(def active-signals {})
(def default-signal-f nil)
(def registered-signals {})

(declare run-with-deref-capture)
(declare notify-deref-watcher)
(declare unbind-dependency)

(defprotocol IParent
             (-add-child [this child])
             (-remove-child [this child])
             (-notify-children [this]))

(defprotocol IChild
             (-add-parent [this parent])
             (-remove-parent [this parent])
             (-unbind-parents [this]))

(defprotocol IRunnable
             (-run [this]))

(defprotocol IRefreshable
             (-refresh [this]))

(defprotocol IDisposable
             (-dispose [this]))

(defprotocol ISubscribable
             (-subscribe [this])
             (-unsubscribe [this]))

(defprotocol IDirty
             (-mark-dirty [this]))

(deftype Derived [path f ^:mutable current-value ^:mutable dirty? ^:mutable subscriptions
                  ^:mutable parents ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 (when dirty?
                       (-refresh this))
                 current-value)
         IRefreshable
         (-refresh [this]
                   (-unbind-parents this)
                   (run-with-deref-capture this)
                   (set! dirty? false))
         IRunnable
         (-run [this]
               (let [value (f)]
                    (when-not (= current-value value)
                              (set! current-value value)
                              (-notify-children this))))
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child)))
         (-notify-children [this]
                           (doseq [child children]
                                  (-mark-dirty child)))
         IDirty
         (-mark-dirty [this]
                      (set! dirty? true))
         IChild
         (-add-parent [this parent]
                      (set! parents (conj parents parent)))
         (-remove-parent [this parent]
                         (set! parents (disj parents parent)))
         (-unbind-parents [this]
                          (doseq [parent parents]
                                 (unbind-dependency parent this)))
         ISubscribable
         (-subscribe [this]
                     (set! subscriptions (inc subscriptions)))
         (-unsubscribe [this]
                       (set! subscriptions (dec subscriptions))
                       (when (= subscriptions 0)
                             (-dispose this)))
         IDisposable
         (-dispose [this]
                   (doseq [parent parents]
                          (-remove-child parent this))
                   (set! parents nil)
                   (set! children nil)
                   (set! current-value nil)
                   (set! active-signals (dissoc active-signals path))))

(deftype Base [parent ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 @parent)
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child)))
         (-notify-children [this]
                           (doseq [child children]
                                  (-refresh child))))

(defn make-base [parent]
      (let [base (Base. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (-notify-children base))))
           base))

(defn make-derived [path f]
      (let [signal (Derived. path f nil true 1 #{} #{})]
           (set! active-signals (assoc active-signals path signal))
           signal))

(defn run-with-deref-capture [signal]
      (if capturing?
        (let [child-signal @signal-being-captured]
             (reset! signal-being-captured signal)
             (-run signal)
             (reset! signal-being-captured child-signal))
        (do (reset! signal-being-captured signal)
            (set! capturing? true)
            (-run signal)
            (set! capturing? false)
            (reset! signal-being-captured nil))))

(defn unbind-dependency [parent child]
      (-remove-parent child parent)
      (-remove-child parent child))

(defn bind-dependency [parent child]
      (-add-parent child parent)
      (-add-child parent child))

(defn notify-deref-watcher [signal]
      (if capturing?
        (bind-dependency signal @signal-being-captured)))

; subscriptions

(defn register-signal [path f]
      (set! registered-signals (assoc-in registered-signals path f)))

(defn register-default-signal-f [f]
      (set! default-signal-f f))

(defn get-in-atom-signal-f [atom]
      (let [base (make-base atom)]
           (fn [path]
               (get-in @base path))))

(defn- path->derived-signal-f [path]
       (loop [node registered-signals path path]
             (cond (map? node)
                   (recur (node (first path)) (rest path))
                   (ifn? node)
                   (node path))))

(defn path->signal-f [path]
      (if-let [f (path->derived-signal-f path)]
              f
              #(default-signal-f path)))

(defn subscribe [path]
      (if-let [signal (active-signals path)]
              (do (-subscribe signal)
                  signal)
              (make-derived path (path->signal-f path))))

(defn unsubscribe [signal]
      (-unsubscribe signal))


