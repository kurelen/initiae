(ns initiae.similarize-test
  (:require
    [clojure.test :refer [deftest are]]
    [initiae.distance :as dist]
    [initiae.similarize :refer [similarize]]))


(defn prob?
  [x]
  (and (<= 0 x) (<= x 1)))


(deftest test-lcs
  (are [x y] (prob? ((similarize dist/levenshtein) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-cosine
  (are [x y] (prob? ((similarize dist/cosine) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-jaccard
  (are [x y] (prob? ((similarize dist/jaccard) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-jaro-winkler
  (are [x y] (prob? ((similarize dist/jaro-winkler) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-levenshtein
  (are [x y] (prob? ((similarize dist/levenshtein) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-levenshtein-fn
  (are [x y] (prob? ((similarize (dist/levenshtein-fn)) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))


(deftest test-weighted-levenshtein-fn
  (are [x y] (prob? ((similarize (dist/levenshtein-fn {:substitute 2})) x y))
    "haus" "haus"
    "haus" "maus"
    "haus" "maut"
    "haus" "mas"))
