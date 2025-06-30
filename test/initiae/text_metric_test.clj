(ns initiae.text-metric-test
  (:require
    [clojure.test :refer [deftest are]]
    [initiae.text-metric :as m]))


(def eps 0.1)


(deftest test-ngram-fn
  (are [x y] (= x y)
    0.0 (m/ngram-sim "haus" "haus")
    0.375 (m/ngram-sim "haus" "maus")
    0.5 (m/ngram-sim "haus" "maut")
    0.75 (m/ngram-sim "haus" "mas")))


(deftest test-ngram-sim-fn
  (are [x y] (< (abs (- x y)) eps)
    0.70 (m/ngram-sim "haus" "park" {:ngram-length 3})
    0.72 (m/ngram-sim "haus" "park" {:ngram-length 4})
    0.25 (m/ngram-sim "haus" "park" {:ngram-length 5})
    0.25 (m/ngram-sim "haus" "park" {:ngram-length 6})))


(deftest test-cosine-dist
  (are [x y] (< (abs (- x y)) eps)
    0 (m/cosine-dist "haus" "haus")
    0.5 (m/cosine-dist "haus" "maus")
    1.0 (m/cosine-dist "haus" "maut")
    1.0 (m/cosine-dist "haus" "mas")))


(deftest test-jaccard-dist
  (are [x y] (< (abs (- x y)) eps)
    0 (m/jaccard-dist "haus" "haus")
    0.66 (m/jaccard-dist "haus" "maus")
    1.0 (m/jaccard-dist "haus" "maut")
    1.0 (m/jaccard-dist "haus" "mas")))


(deftest test-jaro-winkler-dist
  (are [x y] (< (abs (- x y)) eps)
    0 (m/jaro-winkler-dist "haus" "haus")
    0.166 (m/jaro-winkler-dist "haus" "maus")
    0.333 (m/jaro-winkler-dist "haus" "maut")
    0.277 (m/jaro-winkler-dist "haus" "mas")))


(deftest test-levenshtein-dist
  (are [x y] (= x y)
    0.0 (m/levenshtein-dist "haus" "haus")
    1.0 (m/levenshtein-dist "haus" "maus")
    2.0 (m/levenshtein-dist "haus" "maut")
    2.0 (m/levenshtein-dist "haus" "mas")))


(defn prob?
  [x]
  (and (<= 0 x) (<= x 1)))


(deftest test-lcs-sim
  (are [x y] (prob? (m/lcs-sim x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-cosine-sim
  (are [x y] (prob? (m/cosine-sim x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-jaccard-sim
  (are [x y] (prob? (m/jaccard-sim x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-jaro-winkler-sim
  (are [x y] (prob? (m/jaro-winkler-sim x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-levenshtein-sim
  (are [x y] (prob? (m/levenshtein-sim x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))
