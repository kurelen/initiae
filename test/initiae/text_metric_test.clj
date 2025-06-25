(ns initiae.text-metric-test
  (:require
    [clojure.test :refer [deftest are is]]
    [initiae.text-metric :as m]))


(def eps 0.1)


(deftest test-lcs
  (are [x y] (= x y)
    0 (m/levenshtein-dist "haus" "haus")
    1 (m/levenshtein-dist "haus" "maus")
    2 (m/levenshtein-dist "haus" "maut")
    2 (m/levenshtein-dist "haus" "mas")))


(deftest test-cosine
  (are [x y] (< (abs (- x y)) eps)
    0 (m/cosine-dist "haus" "haus")
    1 (m/cosine-dist "haus" "maus")
    1 (m/cosine-dist "haus" "maut")
    1 (m/cosine-dist "haus" "mas")))


(deftest test-jaccard
  (are [x y] (< (abs (- x y)) eps)
    0 (m/jaccard-dist "haus" "haus")
    0.4 (m/jaccard-dist "haus" "maus")
    0.666 (m/jaccard-dist "haus" "maut")
    0.6 (m/jaccard-dist "haus" "mas")))


(deftest test-jaro-winkler
  (are [x y] (< (abs (- x y)) eps)
    0 (m/jaro-winkler-dist "haus" "haus")
    0.166 (m/jaro-winkler-dist "haus" "maus")
    0.333 (m/jaro-winkler-dist "haus" "maut")
    0.277 (m/jaro-winkler-dist "haus" "mas")))


(deftest test-levenshtein
  (are [x y] (= x y)
    0 (m/levenshtein-dist "haus" "haus")
    1 (m/levenshtein-dist "haus" "maus")
    2 (m/levenshtein-dist "haus" "maut")
    2 (m/levenshtein-dist "haus" "mas")))


(deftest test-levenshtein-fn
  (is (= 1 ((m/levenshtein-dist-fn) "haus" "maus")))
  (is (= 2 ((m/levenshtein-dist-fn {:substitute 2}) "haus" "maus"))))
