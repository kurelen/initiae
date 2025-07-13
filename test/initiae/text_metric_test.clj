(ns initiae.text-metric-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [initiae.text-metric :as metric]))


(deftest test-ngram-distance-and-similarity
  (testing "N-gram distance"
    (is (= 0.0 (metric/ngram-dist "hello" "hello")))
    (is (> (metric/ngram-dist "hello" "world") 0))
    (is (> (metric/ngram-dist "abc" "xyz") 0)))

  (testing "N-gram similarity"
    (is (= 1.0 (metric/ngram-sim "hello" "hello")))
    (is (< (metric/ngram-sim "hello" "world") 1.0))
    (is (>= (metric/ngram-sim "abc" "def") 0.0)))

  (testing "N-gram with custom length"
    (let [dist2 (metric/ngram-dist "abc" "abd" {:ngram-length 2})
          dist3 (metric/ngram-dist "abc" "abd" {:ngram-length 3})]
      (is (number? dist2))
      (is (number? dist3)))))


(deftest test-lcs-distance-and-similarity
  (testing "LCS distance"
    (is (= 0 (metric/lcs-dist "hello" "hello")))
    (is (> (metric/lcs-dist "abc" "def") 0))
    (is (= 2 (metric/lcs-dist "ab" "cd")))) ; no common subsequence

  (testing "LCS similarity"
    (is (> (metric/lcs-sim "hello" "hello") 0.5))
    (is (< (metric/lcs-sim "abc" "xyz") 0.5))))


(deftest test-cosine-distance-and-similarity
  (testing "Cosine distance"
    (is (= 0.0 (metric/cosine-dist "hello world" "hello world")))
    (is (>= (metric/cosine-dist "hello" "world") 0.0))
    (is (<= (metric/cosine-dist "hello" "world") 1.0)))

  (testing "Cosine similarity"
    (is (= 1.0 (metric/cosine-sim "hello world" "hello world")))
    (is (>= (metric/cosine-sim "hello" "world") 0.0))
    (is (<= (metric/cosine-sim "hello" "world") 1.0))))


(deftest test-jaccard-distance-and-similarity
  (testing "Jaccard distance"
    (is (= 0.0 (metric/jaccard-dist "hello" "hello")))
    (is (> (metric/jaccard-dist "abc" "def") 0)))

  (testing "Jaccard similarity"
    (is (= 1.0 (metric/jaccard-sim "hello" "hello")))
    (is (< (metric/jaccard-sim "abc" "def") 1.0)))

  (testing "Jaccard with custom shingle size"
    (let [dist (metric/jaccard-dist "hello" "hallo" {:shingle-size 2})
          sim (metric/jaccard-sim "hello" "hallo" {:shingle-size 2})]
      (is (number? dist))
      (is (number? sim))
      (is (>= sim 0.0))
      (is (<= sim 1.0)))))


(deftest test-jaro-winkler-distance-and-similarity
  (testing "Jaro-Winkler distance"
    (is (= 0.0 (metric/jaro-winkler-dist "hello" "hello")))
    (is (> (metric/jaro-winkler-dist "abc" "xyz") 0))
    (is (<= (metric/jaro-winkler-dist "hello" "hallo") 1.0)))

  (testing "Jaro-Winkler similarity"
    (is (= 1.0 (metric/jaro-winkler-sim "hello" "hello")))
    (is (< (metric/jaro-winkler-sim "abc" "xyz") 1.0))
    (is (>= (metric/jaro-winkler-sim "hello" "hallo") 0.0)))

  (testing "Jaro-Winkler with custom threshold"
    (let [dist (metric/jaro-winkler-dist "hello" "hallo" {:threshold 0.8})
          sim (metric/jaro-winkler-sim "hello" "hallo" {:threshold 0.8})]
      (is (number? dist))
      (is (number? sim)))))


(deftest test-levenshtein-distance-and-similarity
  (testing "Levenshtein distance"
    (is (= 0 (metric/levenshtein-dist "hello" "hello")))
    (is (= 1 (metric/levenshtein-dist "cat" "bat")))
    (is (= 3 (metric/levenshtein-dist "kitten" "sitting"))))

  (testing "Levenshtein similarity"
    (is (= 1.0 (metric/levenshtein-sim "hello" "hello")))
    (is (= (/ 2 3) (metric/levenshtein-sim "cat" "bat"))) ; 1 edit, max length 3
    (is (> (metric/levenshtein-sim "kitten" "sitting") 0.5)))

  (testing "Levenshtein with limit"
    (let [dist (metric/levenshtein-dist "very long string" "another very long string" {:limit 5})]
      (is (<= dist 5))))

  (testing "Levenshtein edge cases"
    (is (= 1.0 (metric/levenshtein-sim "" "")))
    (is (= 0.0 (metric/levenshtein-sim "" "hello")))
    (is (= 0.0 (metric/levenshtein-sim "hello" "")))))


(deftest test-damerau-distance-and-similarity
  (testing "Damerau-Levenshtein distance"
    (is (= 0 (metric/damerau-dist "hello" "hello")))
    (is (= 1 (metric/damerau-dist "cat" "bat")))
    (is (= 1 (metric/damerau-dist "ca" "ac"))) ; transposition
    (is (> (metric/damerau-dist "abc" "xyz") 0)))

  (testing "Damerau-Levenshtein similarity"
    (is (= 1.0 (metric/damerau-sim "hello" "hello")))
    (is (= (/ 1 2) (metric/damerau-sim "ca" "ac"))) ; 1 edit, max length 2
    (is (< (metric/damerau-sim "abc" "xyz") 1.0))))


(deftest test-weighted-levenshtein
  (testing "Weighted Levenshtein distance function creation"
    (let [simple-costs {:substitute (fn [c1 c2] (if (= c1 c2) 0.0 1.0))
                        :insert (constantly 1.0)
                        :delete (constantly 1.0)}
          dist-fn (metric/weighted-levenshtein-dist-fn simple-costs)]

      (is (fn? dist-fn))
      (is (= 0.0 (dist-fn "hello" "hello")))
      (is (= 1.0 (dist-fn "cat" "bat")))
      (is (> (dist-fn "abc" "xyz") 0))))

  (testing "Weighted Levenshtein similarity function creation"
    (let [simple-costs {:substitute (fn [c1 c2] (if (= c1 c2) 0.0 1.0))}
          sim-fn (metric/weighted-levenshtein-sim-fn simple-costs)]

      (is (fn? sim-fn))
      (is (= 1.0 (sim-fn "hello" "hello")))
      (is (< (sim-fn "abc" "xyz") 1.0))
      (is (>= (sim-fn "abc" "xyz") 0.0))))

  (testing "Custom substitution costs"
    (let [vowel-costs {:substitute (fn [c1 c2]
                                     (cond
                                       (= c1 c2) 0.0
                                       (and (contains? #{\a \e \i \o \u} c1)
                                            (contains? #{\a \e \i \o \u} c2)) 0.5
                                       :else 1.0))}
          sim-fn (metric/weighted-levenshtein-sim-fn vowel-costs)]

      ;; Vowel substitutions should be cheaper
      (is (> (sim-fn "cat" "cet") (sim-fn "cat" "cxt"))))))


(deftest test-utility-functions
  (testing "String normalization"
    (let [[s1 s2] (metric/normalize-strings "  Hello  " "  WORLD  ")]
      (is (= "hello" s1))
      (is (= "world" s2))))

  (testing "Batch similarity"
    (let [sim-fn (fn [s1 s2] (if (= s1 s2) 1.0 0.5))
          target "hello"
          strings ["hello" "world" "hello"]
          similarities (metric/batch-similarity sim-fn target strings)]

      (is (= [1.0 0.5 1.0] similarities))))

  (testing "Find most similar"
    (let [sim-fn (fn [s1 s2]
                   (- 1.0 (/ (metric/levenshtein-dist s1 s2)
                             (max (count s1) (count s2)))))
          target "hello"
          strings ["hello" "hallo" "world" "hell"]
          result (metric/find-most-similar sim-fn target strings)]

      (is (= 1.0 (:similarity result)))
      (is (= [0] (:indices result)))
      (is (= ["hello"] (:strings result))))))


(deftest test-metric-properties
  (testing "Distance metric properties"
    (let [strings ["hello" "world" "test"]]

      ;; Test non-negativity
      (doseq [s1 strings
              s2 strings]
        (is (>= (metric/levenshtein-dist s1 s2) 0))
        (is (>= (metric/damerau-dist s1 s2) 0)))

      ;; Test identity (distance from string to itself is 0)
      (doseq [s strings]
        (is (= 0 (metric/levenshtein-dist s s)))
        (is (= 0 (metric/damerau-dist s s))))

      ;; Test symmetry
      (doseq [s1 strings
              s2 strings]
        (is (= (metric/levenshtein-dist s1 s2) (metric/levenshtein-dist s2 s1)))
        (is (= (metric/damerau-dist s1 s2) (metric/damerau-dist s2 s1))))))

  (testing "Similarity metric properties"
    (let [strings ["hello" "world" "test"]]

      ;; Test boundedness [0,1]
      (doseq [s1 strings
              s2 strings]
        (is (>= (metric/levenshtein-sim s1 s2) 0.0))
        (is (<= (metric/levenshtein-sim s1 s2) 1.0))
        (is (>= (metric/damerau-sim s1 s2) 0.0))
        (is (<= (metric/damerau-sim s1 s2) 1.0)))

      ;; Test identity (similarity of string to itself is 1)
      (doseq [s strings]
        (is (= 1.0 (metric/levenshtein-sim s s)))
        (is (= 1.0 (metric/damerau-sim s s))))

      ;; Test symmetry
      (doseq [s1 strings
              s2 strings]
        (is (= (metric/levenshtein-sim s1 s2) (metric/levenshtein-sim s2 s1)))
        (is (= (metric/damerau-sim s1 s2) (metric/damerau-sim s2 s1)))))))


(deftest test-medieval-text-examples
  (testing "Medieval manuscript variations"
    ;; Test cases that might appear in medieval manuscripts
    (let [variations [["Ave maria gratia plena" "Ave maria gracia plena"]
                      ["Pange lingua gloriosi" "Pangue lingua gloriosj"]
                      ["veni creator spiritus" "ueni creator spiritus"]]

          lev-sim metric/levenshtein-sim]

      (doseq [[original variant] variations]
        ;; Variants should have high similarity
        (is (> (lev-sim original variant) 0.7))
        ;; Should be symmetric
        (is (= (lev-sim original variant) (lev-sim variant original)))))))


(deftest test-edge-cases-and-errors
  (testing "Empty string handling"
    (is (= 0.0 (metric/ngram-dist "" "")))
    (is (= 0 (metric/levenshtein-dist "" "")))
    (is (= 1.0 (metric/levenshtein-sim "" "")))
    (is (= 1.0 (metric/cosine-sim "" "")))
    (is (= 1.0 (metric/jaccard-sim "" ""))))

  (testing "Single character strings"
    (is (= 0 (metric/levenshtein-dist "a" "a")))
    (is (= 1 (metric/levenshtein-dist "a" "b")))
    (is (= 1.0 (metric/levenshtein-sim "a" "a")))
    (is (= 0.0 (metric/levenshtein-sim "a" "b"))))

  (testing "Very long strings don't cause errors"
    (let [long-string (apply str (repeat 1000 "a"))
          other-string (apply str (repeat 1000 "b"))]
      (is (number? (metric/levenshtein-dist long-string other-string)))
      (is (number? (metric/jaro-winkler-sim long-string other-string))))))


(deftest test-consistency-between-distance-and-similarity
  (testing "Distance and similarity should be consistent"
    (let [test-pairs [["hello" "hello"]
                      ["hello" "hallo"]
                      ["abc" "xyz"]
                      ["" ""]
                      ["a" ""]]]

      (doseq [[s1 s2] test-pairs]
        ;; For Levenshtein: sim = 1 - (dist / max_length)
        (let [dist (metric/levenshtein-dist s1 s2)
              sim (metric/levenshtein-sim s1 s2)
              max-len (max (count s1) (count s2))
              expected-sim (if (zero? max-len) 1.0 (- 1.0 (/ dist max-len)))]
          (is (< (Math/abs (- sim expected-sim)) 1e-10)))

        ;; For Damerau: sim = 1 - (dist / max_length)
        (let [dist (metric/damerau-dist s1 s2)
              sim (metric/damerau-sim s1 s2)
              max-len (max (count s1) (count s2))
              expected-sim (if (zero? max-len) 1.0 (- 1.0 (/ dist max-len)))]
          (is (< (Math/abs (- sim expected-sim)) 1e-10)))))))
