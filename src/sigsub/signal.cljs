(ns sigsub.signal)

(def capturing? false)
(def signal-being-captured (atom nil))

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

(deftype Derived [f ^:mutable current-value ^:mutable parents ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 current-value)
         IRefreshable
         (-refresh [this]
                   (-unbind-parents this)
                   (run-with-deref-capture this))
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
                        (set! children (disj children child))
                        (if (empty? children)
                          (-dispose this)))
         (-notify-children [this]
                           (doseq [child children]
                                  (-refresh child)))
         IChild
         (-add-parent [this parent]
                      (set! parents (conj parents parent)))
         (-remove-parent [this parent]
                         (set! parents (disj parents parent)))
         (-unbind-parents [this]
                          (doseq [parent parents]
                                 (unbind-dependency parent this)))

         IDisposable
         (-dispose [this]
                   (doseq [parent parents]
                          (-remove-child parent this))
                   (set! parents nil)
                   (set! children nil)
                   (set! current-value nil)))

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

(defn make-derived [f]
      (let [signal (Derived. f nil #{} #{})]
           (-refresh signal)
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

