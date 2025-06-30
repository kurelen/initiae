(ns initiae.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.core :refer [load-fixture-edn flatten-fixture]]))


(deftest test-fixture-edn
  (let [data (load-fixture-edn)]
    (is (vector? data))
    (doseq [entry data]
      (doseq [[_ value] entry]
        (is (vector? value))
        (is (every? string? value))))))


(deftest test-flatten-fixture
  (is (= [1 2 3]
         (flatten-fixture [{:a [1]} {:b [2 3]}]))))
