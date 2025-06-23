(ns initiae.distance-test
  (:require
    [clojure.test :refer [deftest are is]]
    [initiae.distance :as dist]))


(def eps 0.1)


(deftest test-lcs
  (are [x y] (= x y)
    0 (dist/levenshtein "haus" "haus")
    1 (dist/levenshtein "haus" "maus")
    2 (dist/levenshtein "haus" "maut")
    2 (dist/levenshtein "haus" "mas")))


(deftest test-cosine
  (are [x y] (< (abs (- x y)) eps)
    0 (dist/cosine "haus" "haus")
    1 (dist/cosine "haus" "maus")
    1 (dist/cosine "haus" "maut")
    1 (dist/cosine "haus" "mas")))


(deftest test-jaccard
  (are [x y] (< (abs (- x y)) eps)
    0 (dist/jaccard "haus" "haus")
    0.4 (dist/jaccard "haus" "maus")
    0.666 (dist/jaccard "haus" "maut")
    0.6 (dist/jaccard "haus" "mas")))


(deftest test-jaro-winkler
  (are [x y] (< (abs (- x y)) eps)
    0 (dist/jaro-winkler "haus" "haus")
    0.166 (dist/jaro-winkler "haus" "maus")
    0.333 (dist/jaro-winkler "haus" "maut")
    0.277 (dist/jaro-winkler "haus" "mas")))


(deftest test-levenshtein
  (are [x y] (= x y)
    0 (dist/levenshtein "haus" "haus")
    1 (dist/levenshtein "haus" "maus")
    2 (dist/levenshtein "haus" "maut")
    2 (dist/levenshtein "haus" "mas")))


(deftest test-levenshtein-fn
  (is (= 1 ((dist/levenshtein-fn) "haus" "maus")))
  (is (= 2 ((dist/levenshtein-fn {:substitute 2}) "haus" "maus"))))


(deftest test-distance-matrix
  (is (= [[0]] (dist/distance-matrix ["haus"] dist/levenshtein)))
  (is (= [[0 1] [1 0]] (dist/distance-matrix ["haus" "maus"] dist/levenshtein))))
