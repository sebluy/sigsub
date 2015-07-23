(ns test.sigsub.core
  (:require [sigsub.signal :as signal]
            [cljs.test :refer-macros [deftest is run-tests testing]]))

(deftest base-signal
         (let [a (atom 1)
               b (signal/make-base a)]
              (is (= @b 1))
              (reset! a 2)
              (is (= @b 2))))

(deftest derived-signal
         (let [a (atom 1)
               b (signal/make-base a)
               c (signal/make-derived #(+ @b 1))
               d (signal/make-derived #(+ @c 1))
               e (signal/make-derived #(if (= @b 1) @c @d))]
              (is (= @c 2))
              (is (= @d 3))
              (is (= @e 2))
              (reset! a 2)
              (is (= @c 3))
              (is (= @d 4))
              (is (= @e 4))))

(deftest derived-signal-computed-when-neccesary
         (let [computed (atom 0)
               a (atom {:important 1 :useless 2})
               b (signal/make-base a)
               c (signal/make-derived #(@b :important))
               d (signal/make-derived
                   (fn [] (swap! computed inc) (+ @c 1)))]
              (is (= @computed 0))
              (swap! a assoc :useless 3)
              (is (= @computed 0))
              (is (= @d 2))
              (is (= @computed 1))
              (swap! a assoc :useless 4)
              (is (= @d 2))
              (is (= @computed 1))))

(run-tests)
