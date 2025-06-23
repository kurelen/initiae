(ns initiae.distance-test
  (:require
    [clojure.test :refer [deftest are]]
    [initiae.distance :refer [weighted-levenshtein]]))


(deftest test-weighted-levenshtein
  (are [x y] (= x y) 
       0 (weighted-levenshtein "haus" "haus")
       1 (weighted-levenshtein "haus" "maus")
       2 (weighted-levenshtein "haus" "maut")
       2 (weighted-levenshtein "haus" "mas")))
