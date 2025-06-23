(ns initiae.distance-test
  (:require
    [clojure.test :refer [deftest are is]]
    [initiae.distance :refer [weighted-levenshtein distance-matrix]]))


(deftest test-weighted-levenshtein
  (are [x y] (= x y)
    0 (weighted-levenshtein "haus" "haus")
    1 (weighted-levenshtein "haus" "maus")
    2 (weighted-levenshtein "haus" "maut")
    2 (weighted-levenshtein "haus" "mas")))


(deftest test-distance-matrix
  (is (= [[0]] (distance-matrix ["haus"] weighted-levenshtein)))
  (is (= [[0 1] [1 0]] (distance-matrix ["haus" "maus"] weighted-levenshtein))))
