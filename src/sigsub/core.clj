(ns sigsub.core)

(defn- apply-fn-to-bindings [bindings f]
  (into []
        (apply concat
               (map (fn [[symbol arg]]
                      [symbol (list f arg)])
                    (partition 2 bindings)))))

(defn- reagent-subscribe-bindings [bindings]
  (apply-fn-to-bindings bindings 'sigsub.core/subscribe-reagent))

(defn- signal-reference-bindings [bindings]
  (apply-fn-to-bindings bindings 'sigsub.core/reference))

(defmacro with-reagent-subs [bindings fn]
  (let [subscriptions (reagent-subscribe-bindings bindings)]
    `(let ~subscriptions
       ~fn)))

(defmacro with-signals [bindings fn]
  (let [signals (signal-reference-bindings bindings)]
    `(let ~signals
       ~fn)))

