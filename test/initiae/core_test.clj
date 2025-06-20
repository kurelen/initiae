(ns initiae.core-test
  (:require
    [clojure.test :refer :all]
    [initiae.core :refer :all]))


(deftest test-greet
  (is (= "Hello, Magda!" (greet "Magda")))
  (is (not (= "Hello, Alex!" (greet "Magda")))))
