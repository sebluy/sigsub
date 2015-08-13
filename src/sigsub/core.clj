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
          "Convenience macro for combining let with subscribe-reagent.
          Used inside a reagent component function.
          Example:
          (let [a (sigsub/subscribe-reagent (fn [] [:a]))
                b (sigsub/subscribe-reagent (fn [] [:b])]
                (fn [] (+ @a @b)))
          Can be replaced by:
          (sigsub/with-reagent-subs
            [a [:a]
             b [:b]
             (fn [] (+ @a @b)))"
          (let [subscriptions (reagent-subscribe-bindings bindings)]
               `(let ~subscriptions
                     ~fn)))

(defmacro with-signals [bindings fn]
          "Similar to with-reagent-subs, only replacing subscribe-reagent
          with reference.
          Used for creating signal dependencies inside a derived signal
          run function."
          (let [signals (signal-reference-bindings bindings)]
               `(let ~signals
                     ~fn)))

