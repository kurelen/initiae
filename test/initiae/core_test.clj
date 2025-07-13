(ns initiae.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.core :as core]))


(deftest test-load-initiae
  (let [data (core/load-initiae)]
    (is (vector? data))
    (is (not (= 0 (count data))))))

