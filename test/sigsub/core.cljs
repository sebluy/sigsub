(ns test.sigsub.core
  (:require [sigsub.signal :as signal]
            [cljs.test :refer-macros [deftest is run-tests testing]]))

(deftest base-signal
         (let [a (atom 1)
               b (signal/make-base a)]
              (is (= @b 1))
              (reset! a 2)
              (is (= @b 2))))

(deftest default-derived-signal
         (set! signal/active-signals {})
         (let [a (atom {:test {:value 1}})]
              (signal/register-default-signal-f (signal/get-in-atom-signal-f a))
              (let [c (signal/subscribe [:test :value])]
                   (is (= @c 1))
                   (swap! a assoc-in [:test :value] 2)
                   (is (= @c 2)))))

(deftest registered-derived-signal
         (set! signal/active-signals {})
         (set! signal/registered-signals {})
         (let [a (atom {:test 1})]
              (signal/register-default-signal-f (signal/get-in-atom-signal-f a))
              (signal/register-signal
                [:b]
                (fn [] (let [a (signal/subscribe [:test])]
                            #(+ @a 1))))
              (signal/register-signal
                [:c]
                (fn [] (let [b (signal/subscribe [:b])]
                            #(+ @b 1))))
              (let [b (signal/subscribe [:b])
                    c (signal/subscribe [:c])]
                   (is (= @b 2))
                   (is (= @c 3))
                   (reset! a {:test 2})
                   (is (= @b 3))
                   (is (= @c 4)))))

#_(deftest derived-signal-computed-when-neccesary
         (let [computed (atom 0)
               a (atom {:important 1 :useless 2})
               b (signal/make-base a)
               c (signal/make-derived #(@b :important))
               d (signal/make-derived
                   (fn [] (swap! computed inc) (+ @c 1)))]
              (is (= @computed 1))
              (swap! a assoc :useless 3)
              (is (= @computed 1))
              (is (= @d 2))
              (is (= @computed 1))
              (swap! a assoc :useless 4)
              (is (= @d 2))
              (is (= @computed 1))))

#_(deftest derived-signal-parent-child-maintained
         (let [a (atom 1)
               b (signal/make-base a)
               c (signal/make-derived #(+ @b 1))
               d (signal/make-derived #(if (= @b 1) @c 3))]
              (is (= @d 2))
              (is (= (.-parents d) #{b c}))
              (is (= (.-children c) #{d}))
              (reset! a 2)
              (is (= @d 3))
              (is (= (.-parents d) #{b}))
              (is (= (.-children c) #{}))))

;(set! signal/active-signals {})
;(set! signal/registered-signals {})

;(keys signal/registered-signals)
#_(let [a (atom {:test 1})]
     (signal/register-default-signal-f (signal/get-in-atom-signal-f a))
     (signal/register-signal
       [:b]
       (fn [] (let [a (signal/subscribe [:test])]
                   #(+ @a 1))))
     (signal/register-signal
       [:c]
       (fn [] (let [b (signal/subscribe [:b])]
                   #(+ @b 1)))))

;(def b (signal/subscribe [:b]))
;(.-f b)
;(def c (signal/subscribe [:c]))

;(+ 1 1)
;@b

(run-tests)
