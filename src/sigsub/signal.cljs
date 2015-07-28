(ns sigsub.signal
  (:require [clojure.set :as set]))

(def capturing? false)
(def captured-parents nil)
(def signal-being-captured nil)

(declare run-with-deref-capture)
(declare notify-deref-watcher)
(declare bind-dependency)
(declare unbind-dependency)

(defprotocol IParent
             (-add-child [this child])
             (-remove-child [this child])
             (-refresh-children [this]))

(defprotocol IChild
             (-update-parents [this parents])
             (-add-parent [this parent])
             (-remove-parent [this parent]))

(defprotocol IRunnable
             (-run [this]))

(defprotocol IRefreshable
             (-refresh [this]))

(defprotocol IActivatable
             (-activate [this])
             (-deactivate [this]))

; add back in subscription reference count
(deftype Derived [path f
                  ^:mutable current-value
                  ^:mutable active?
                  ^:mutable parents ^:mutable children]
         IDeref
         (-deref [this]
                 (notify-deref-watcher this)
                 (when-not active?
                           (-activate this))
                 current-value)
         IRefreshable
         (-refresh [this]
                   (println "Refreshing " path)
                   (let [new-parents (run-with-deref-capture this)]
                        (-update-parents this new-parents)))
         IRunnable
         (-run [this]
               (let [value (f)]
                    (when-not (= current-value value)
                              (set! current-value value)
                              (-refresh-children this))))
         IParent
         (-add-child [this child]
                     (set! children (conj children child)))
         (-remove-child [this child]
                        (set! children (disj children child))
                        (if (= (count children) 0)
                          (-deactivate this)))
         (-refresh-children [this]
                            (doseq [child children]
                                   (-refresh child)))
         IChild
         (-update-parents [this new-parents]
                          (let [to-remove (set/difference parents new-parents)
                                to-add (set/difference new-parents parents)]
                               (doseq [retired-parent to-remove]
                                      (unbind-dependency retired-parent this))
                               (doseq [new-parent to-add]
                                      (bind-dependency new-parent this))))
         (-add-parent [this parent]
                      (set! parents (conj parents parent)))
         (-remove-parent [this parent]
                         (set! parents (disj parents parent)))
         IActivatable
         (-activate [this]
                    (set! active? true)
                    (set! parents #{})
                    (set! children #{})
                    (-refresh this))
         (-deactivate [this]
                      (-update-parents this #{})
                      (set! active? false)
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
         (-refresh-children [this]
                            (doseq [child children]
                                   (-refresh child))))

(defn make-base [parent]
      (let [base (Base. parent #{})]
           (-add-watch parent base
                       (fn [_ old-val new-val]
                           (if (not= old-val new-val)
                             (-refresh-children base))))
           base))

(defn make-derived [path f]
      (Derived. path f nil false nil nil))

(defn run-with-deref-capture [signal]
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

(defn unbind-dependency [parent child]
      (println "unbinding " (.-path parent) (.-path child))
      (-remove-parent child parent)
      (-remove-child parent child))

(defn bind-dependency [parent child]
      (println "binding " (.-path parent) (.-path child))
      (-add-parent child parent)
      (-add-child parent child))

(defn notify-deref-watcher [signal]
      (when capturing?
            (set! captured-parents (conj captured-parents signal))))

