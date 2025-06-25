(ns initiae.matrix-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.matrix :as matrix]))


(deftest test-gen-symmetric
  (is (= [[0 0 0] [0 1 1] [0 1 2]]
         (matrix/gen-symmetric (fn [a _] a) (range 3)))))
