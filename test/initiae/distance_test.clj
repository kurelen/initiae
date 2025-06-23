(ns initiae.distance-test
  (:require
    [clojure.test :refer [deftest are is]]
    [initiae.distance :refer [weighted-levenshtein levenshtein distance-matrix]]))


(deftest test-levenshtein
  (are [x y] (= x y)
    0 (levenshtein "haus" "haus")
    1 (levenshtein "haus" "maus")
    2 (levenshtein "haus" "maut")
    2 (levenshtein "haus" "mas")))


(deftest test-distance-matrix
  (is (= [[0]] (distance-matrix ["haus"] levenshtein)))
  (is (= [[0 1] [1 0]] (distance-matrix ["haus" "maus"] levenshtein))))


(deftest test-weighted-levenshtein
  (is (= 1 ((weighted-levenshtein) "haus" "maus")))
  (is (= 2 ((weighted-levenshtein {:substitute 2}) "haus" "maus"))))
