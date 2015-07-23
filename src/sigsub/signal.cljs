(ns sigsub.signal)

(def capturing? false)
(def signal-being-captured (atom nil))

(declare run-with-deref-capture)
(declare notify-deref-watcher)

(defprotocol IParent
             (-add-child [this child])
             (-remove-child [this child])
             (-notify-children [this]))

(defprotocol IChild
             (-add-parent [this parent])
             (-remove-parent [this parent])
             (-clear-parents [this]))

(defprotocol IRunnable
             (-run [this]))

(defprotocol IDirty
             (-mark-dirty [this]))

(deftype Derived [f ^:mutable current-value ^:mutable dirty? ^:mutable parents ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 (if-not dirty?
                         current-value
                         (do
                           (run-with-deref-capture this)
                           current-value)))
         IRunnable
         (-run [this]
               (let [value (f)]
                    (when-not (= current-value value)
                              (set! current-value value)
                              (-notify-children this))
                    (set! dirty? false)))
         IDirty
         (-mark-dirty [this]
                      (set! dirty? true))
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child)))
         (-notify-children [this]
                           (doseq [child children]
                                  (-mark-dirty child)))
         IChild
         (-add-parent [this parent]
                      (set! parents (conj parents parent)))
         (-remove-parent [this parent]
                         (set! parents (disj parents parent)))
         (-clear-parents [this]
                         (set! parents #{})))

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
                                  (-mark-dirty child))))

(defn make-base [parent]
      (let [base (Base. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (-notify-children base))))
           base))

(defn make-derived [f]
      (Derived. f nil true #{} #{}))

(defn run-with-deref-capture [signal]
      (if capturing?
        (let [child-signal @signal-being-captured]
             (reset! signal-being-captured signal)
             (-clear-parents signal)
             (-run signal)
             (reset! signal-being-captured child-signal))
        (do (reset! signal-being-captured signal)
            (set! capturing? true)
            (-clear-parents signal)
            (-run signal)
            (set! capturing? false)
            (reset! signal-being-captured nil))))

(defn bind-dependency [parent child]
      (-add-parent child parent)
      (-add-child parent child))

(defn notify-deref-watcher [signal]
      (if capturing?
        (bind-dependency signal @signal-being-captured)))

