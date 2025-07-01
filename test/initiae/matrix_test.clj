(ns initiae.matrix-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.matrix :as matrix]))


(deftest test-symmetric
  (is (= [[0 1 2] [1 2 3] [2 3 4]]
         (matrix/symmetric + (range 3)))))

(deftest test-pairwise-arity-2
  (is (= [[0 2 4] [1 3 5] [2 4 6]]
         (matrix/pairwise 
           (fn [a b] (+ a (* 2 b)))
           (range 3)))))

(deftest test-pairwise-arity-3
  (is (= [[0 2 4] [1 3 5]]
         (matrix/pairwise 
           (fn [a b] (+ a (* 2 b)))
                          (range 2) (range 3)))))
