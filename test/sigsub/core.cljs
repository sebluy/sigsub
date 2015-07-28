(ns test.sigsub.core
  (:require [sigsub.core :as sigsub]
            [sigsub.signal :as signal]
            [sigsub.manager :as manager]
            [cljs.test :refer-macros [deftest is run-tests testing]]))

(defn setup [db]
      (set! manager/current-signals {})
      (set! manager/current-signal-counts {})
      (set! manager/registered-derived-signal-fns {})
      (manager/register-default-signal-fn
        (manager/get-in-atom-signal-fn db)))

(deftest base-signal
         (let [a (atom 1)
               b (signal/make-base a)]
              (is (= @b 1))
              (reset! a 2)
              (is (= @b 2))))

(deftest default-derived-signal
         (let [db (atom {:test 1})]
              (setup db)
              (let [c (sigsub/get-signal [:test])]
                   (is (= @c 1))
                   (swap! db assoc-in [:test] 2)
                   (is (= @c 2)))))

(defn inc-path [path count]
      (fn []
          (let [signal (sigsub/get-signal path)]
               (fn []
                   (swap! count inc)
                   (+ @signal 1)))))

(deftest registered-derived-signal
         (let [db (atom {:a 1})
               b-count (atom 0)
               c-count (atom 0)]
              (setup db)
              (manager/register-derived-signal-fn
                [:b]
                (inc-path [:a] b-count))
              (manager/register-derived-signal-fn
                [:c]
                (inc-path [:b] c-count))
              (let [b (sigsub/get-signal [:b])
                    c (sigsub/get-signal [:c])]

                   (testing "does not compute on creation"
                            (is (= @b-count 0)))

                   (testing "computes correct value on deref"
                            (is (= @b 2))
                            (is (= @b-count 1)))

                   (testing "uses cached value"
                            (is (= @b 2))
                            (is (= @b-count 1)))

                   (testing (str "chained signals compute correct value "
                                 "and do not cause additional computation")
                            (is (= @c 3))
                            (is (= @c-count 1))
                            (is (= @b-count 1)))

                   (reset! db {:a 2})

                   (testing "recomputes on dependency change"
                            (is (= @b 3))
                            (is (= @b-count 2))
                            (is (= @c 4))
                            (is (= @c-count 2))))))

(deftest manager-test
         (let [db (atom {:a 1})
               b-count (atom 0)
               c-count (atom 0)]
              (setup db)
              (manager/register-derived-signal-fn
                [:b]
                (inc-path [:a] b-count))
              (manager/register-derived-signal-fn
                [:c]
                (inc-path [:b] c-count))
              (let [c (sigsub/get-signal [:c])
                    b (sigsub/get-signal [:b])]
                   (testing "signals are reused"
                            (is (= c (sigsub/get-signal [:c]))))
                   (testing "signals are not retained"
                            (sigsub/dispose-signal [:b])
                            (dotimes [_ 2]
                                     (sigsub/dispose-signal [:c]))
                            (is (not= c (sigsub/get-signal [:c])))
                            (is (not= b (sigsub/get-signal [:b])))))))

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

;(run-tests)
(cljs.test/test-vars [#'test.sigsub.core/registered-derived-signal])
