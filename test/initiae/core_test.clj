(ns initiae.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.core :refer [load-fixture flatten-fixture]]))


(deftest test-fixture
  (let [data (load-fixture)]
    (is (vector? data))
    (doseq [entry data]
      (doseq [[_ value] entry]
        (is (vector? value))
        (is (every? string? value))))))


(deftest test-flatten-fixture
  (is (= [1 2 3]
         (flatten-fixture [{:a [1]} {:b [2 3]}]))))
