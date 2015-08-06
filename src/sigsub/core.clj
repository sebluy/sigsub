(ns sigsub.core)

(defn- map-bindings [bindings f]
       (into []
             (apply concat
                    (map f (partition 2 bindings)))))

(defn- map-f-wrap-fn-bindings [bindings f]
       (map-bindings bindings
                     (fn [[symbol arg]]
                         [symbol `(~f (fn [] ~arg))])))

(defn- reagent-subscribe-bindings [bindings]
       (map-f-wrap-fn-bindings bindings 'sigsub.core/subscribe-reagent))

(defn- signal-reference-bindings [bindings]
       (map-f-wrap-fn-bindings bindings 'sigsub.core/reference))

(defmacro with-reagent-subs [bindings fn]
          (let [subscriptions (reagent-subscribe-bindings bindings)]
               `(let ~subscriptions
                     ~fn)))

(defmacro with-signals [bindings fn]
          (let [signals (signal-reference-bindings bindings)]
               `(let ~signals
                     ~fn)))

