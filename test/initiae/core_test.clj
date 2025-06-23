(ns initiae.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.core :refer [load-fixture]]))


(deftest test-fixture
  (let [data (load-fixture)]
    (is (vector? data))
    (doseq [entry data]
      (doseq [[_ value] entry]
        (is (vector? value))
        (is (every? string? value))))))
