(ns sigsub.core-test
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [reagent.core :as reagent]
            [goog.dom :as dom]
            [sigsub.core :as sigsub]))

(enable-console-print!)

(defn setup [db]
      (set! sigsub/active-signals {})
      (set! sigsub/registered-derived-signal-fns {})
      (sigsub/register-default-signal-fn
        (sigsub/get-in-atom-signal-fn db)))

(deftest base-signal
         (let [a (atom 1)
               b (sigsub/make-base a)]
              (is (= @b 1))
              (reset! a 2)
              (is (= @b 2))))

(deftest default-derived-signal
         (let [db (atom {:test 1})]
              (setup db)
              (let [c (sigsub/subscribe [:test])]
                   (is (= @c 1))
                   (swap! db assoc-in [:test] 2)
                   (is (= @c 2))
                   (sigsub/unsubscribe c))))

(defn inc-path [path count]
      (fn []
          (let [signal (sigsub/reference path)]
               (fn []
                   (swap! count inc)
                   (+ @signal 1)))))

(defn sum-paths [paths count]
      (fn []
          (let [signals (map sigsub/reference paths)]
               (fn []
                   (swap! count inc)
                   (apply + (map deref signals))))))

(defn active-signals-has-paths? [paths]
      (= (into #{} paths)
         (into #{} (keys sigsub/active-signals))))

(defn render [component]
      (reagent/render component (dom/getElement "app"))
      (reagent/flush))

(defn sum-component []
      (let [sum (sigsub/reagent-subscribe [:sum])]
           (fn []
               [:div#sum @sum])))

(deftest registered-derived-signal
         (let [db (atom {:a 1 :b 2})
               inc-a-count (atom 0)
               inc-b-count (atom 0)
               sum-count (atom 0)]
              (setup db)
              (sigsub/register-derived-signal-fn
                [:inc-a]
                (inc-path [:a] inc-a-count))
              (sigsub/register-derived-signal-fn
                [:inc-b]
                (inc-path [:b] inc-b-count))
              (sigsub/register-derived-signal-fn
                [:sum]
                (sum-paths '([:inc-a] [:inc-b]) sum-count))
              (let [inc-a (sigsub/subscribe [:inc-a])
                    inc-b (sigsub/subscribe [:inc-b])
                    sum (sigsub/subscribe [:sum])]

                   (testing "caches signals"
                            (is (active-signals-has-paths?
                                  #{[:a] [:b] [:inc-a] [:inc-b] [:sum]})))

                   (testing "computes on creation"
                            (is (= @inc-a-count 1))
                            (is (= @inc-b-count 1))
                            (is (= @sum-count 1)))

                   (testing "computes correct value on deref"
                            (is (= @inc-a 2))
                            (is (= @inc-a-count 1)))

                   (testing "uses cached value"
                            (is (= @inc-a 2))
                            (is (= @inc-a-count 1)))

                   (testing (str "chained signals compute correct value "
                                 "and do not cause additional computation")
                            (is (= @sum 5))
                            (is (= @sum-count 1))
                            (is (= @inc-a-count 1))
                            (is (= @inc-b-count 1)))


                   (testing "recomputes on dependency change"
                            (swap! db assoc :a 2)
                            (is (= @inc-a 3))
                            (is (= @inc-a-count 2))
                            (is (= @sum 6))
                            (is (= @sum-count 2)))

                   (testing "only recomputes on immediate dependency changes"
                            (is (= @inc-b 3))
                            (is (= @inc-b-count 1)))

                   (testing (str "multiple dependency change"
                                 "does not cause multiple recomputes")
                            (reset! db {:a 3 :b 4})
                            (is (= @sum 9))
                            (is (= @sum-count 3)))

                   (testing "uncaches signals"
                            (sigsub/unsubscribe inc-a)
                            (is (active-signals-has-paths?
                                  #{[:a] [:b] [:inc-a] [:inc-b] [:sum]}))
                            (sigsub/unsubscribe sum)
                            (is (active-signals-has-paths?
                                  #{[:b] [:inc-b]}))
                            (sigsub/unsubscribe inc-b)
                            (is (active-signals-has-paths? #{})))

                   (testing "query"
                            (is (= 9 (sigsub/query [:sum])))
                            (is (active-signals-has-paths? #{})))

                   (testing "reagent integration"
                            (render [sum-component])
                            (is (= (dom/getTextContent (dom/getElement "sum"))
                                   "9"))
                            (reset! db {:a 2 :b 6})
                            (reagent/flush)
                            (is (= (dom/getTextContent (dom/getElement "sum"))
                                   "10"))
                            (render [:div])
                            (is (active-signals-has-paths? #{}))))))

(defn run-all-tests []
      (run-tests))

(run-all-tests)

