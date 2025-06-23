(ns initiae.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.core :refer [greet]])

(deftest test-greet
  (is (= "Hello, Magda!" (greet "Magda")))
  (is (not (= "Hello, Alex!" (greet "Magda")))))
