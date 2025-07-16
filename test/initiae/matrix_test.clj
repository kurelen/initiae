(ns initiae.matrix-test
  (:require
    [clojure.core.matrix :as m]
    [clojure.test :refer [deftest is testing]]
    [initiae.matrix :as matrix]))


(deftest test-symmetric-matrix
  (testing "Symmetric matrix generation"
    (let [v ["a" "b" "c"]
          dist-fn (fn [x y] (if (= x y) 0 1))
          result (matrix/symmetric dist-fn v)]

      ;; Check dimensions
      (is (= 3 (m/row-count result)))
      (is (= 3 (m/column-count result)))

      ;; Check diagonal (should be 0 for distance function)
      (is (= 0.0 (m/mget result 0 0)))
      (is (= 0.0 (m/mget result 1 1)))
      (is (= 0.0 (m/mget result 2 2)))

      ;; Check symmetry
      (is (= (m/mget result 0 1) (m/mget result 1 0)))
      (is (= (m/mget result 0 2) (m/mget result 2 0)))
      (is (= (m/mget result 1 2) (m/mget result 2 1)))

      ;; Check off-diagonal values
      (is (= 1.0 (m/mget result 0 1)))
      (is (= 1.0 (m/mget result 1 2)))))

  (testing "Symmetric matrix with custom function"
    (let [v [1 2 3]
          sum-fn (fn [x y] (+ x y))
          result (matrix/symmetric sum-fn v)]

      (is (= 2.0 (m/mget result 0 0))) ; 1+1
      (is (= 3.0 (m/mget result 0 1))) ; 1+2
      (is (= 4.0 (m/mget result 0 2))) ; 1+3
      (is (= 4.0 (m/mget result 1 1))) ; 2+2
      (is (= 5.0 (m/mget result 1 2))) ; 2+3
      (is (= 6.0 (m/mget result 2 2))) ; 3+3

      ;; Verify symmetry
      (is (= (m/mget result 0 1) (m/mget result 1 0)))
      (is (= (m/mget result 0 2) (m/mget result 2 0)))
      (is (= (m/mget result 1 2) (m/mget result 2 1))))))


(deftest test-pairwise-matrix
  (testing "Pairwise matrix with single vector"
    (let [v [1 2]
          concat-fn (fn [x y] (+ x y))
          result (matrix/pairwise concat-fn v)]

      (is (= 2 (m/row-count result)))
      (is (= 2 (m/column-count result)))
      (is (= 2.0 (m/mget result 0 0)))
      (is (= 3.0 (m/mget result 0 1)))
      (is (= 3.0 (m/mget result 1 0)))
      (is (= 4.0 (m/mget result 1 1)))))

  (testing "Pairwise matrix with two vectors"
    (let [v1 [1 2]
          v2 [3 4 5]
          mult-fn (fn [x y] (* x y))
          result (matrix/pairwise mult-fn v1 v2)]

      (is (= 2 (m/row-count result)))
      (is (= 3 (m/column-count result)))
      (is (= 3.0 (m/mget result 0 0)))  ; 1*3
      (is (= 4.0 (m/mget result 0 1)))  ; 1*4
      (is (= 5.0 (m/mget result 0 2)))  ; 1*5
      (is (= 6.0 (m/mget result 1 0)))  ; 2*3
      (is (= 8.0 (m/mget result 1 1)))  ; 2*4
      (is (= 10.0 (m/mget result 1 2)))))) ; 2*5

(deftest test-matrix-stats
  (testing "Matrix statistics"
    (let [matrix (m/matrix [[1 2 3]
                            [4 5 6]
                            [7 8 9]])
          stats (matrix/matrix-stats matrix)]

      (is (= [3 3] (:shape stats)))
      (is (= 1.0 (:min stats)))
      (is (= 9.0 (:max stats)))
      (is (= 5.0 (:mean stats)))
      (is (false? (:symmetric? stats)))))

  (testing "Symmetric matrix statistics"
    (let [matrix (m/matrix [[1 2 3]
                            [2 4 5]
                            [3 5 6]])
          stats (matrix/matrix-stats matrix)]

      (is (true? (:symmetric? stats))))))


(deftest test-threshold-matrix
  (testing "Thresholding matrix values"
    (let [matrix (m/matrix [[0.1 0.5 0.9]
                            [0.3 0.7 0.2]
                            [0.8 0.4 0.6]])
          thresholded (matrix/threshold-matrix matrix 0.5)]

      (is (= 0.0 (m/mget thresholded 0 0))) ; 0.1 -> 0.0
      (is (= 0.5 (m/mget thresholded 0 1))) ; 0.5 -> 0.5
      (is (= 0.9 (m/mget thresholded 0 2))) ; 0.9 -> 0.9
      (is (= 0.0 (m/mget thresholded 1 0))) ; 0.3 -> 0.0
      (is (= 0.7 (m/mget thresholded 1 1))) ; 0.7 -> 0.7
      (is (= 0.8 (m/mget thresholded 2 0)))))) ; 0.8 -> 0.8

(deftest test-normalize-matrix
  (testing "Matrix normalization"
    (let [matrix (m/matrix [[1 2 3]
                            [4 5 6]
                            [7 8 9]])
          normalized (matrix/normalize-matrix matrix)]

      ;; Check that min value becomes 0
      (is (= 0.0 (m/emin normalized)))
      ;; Check that max value becomes 1
      (is (= 1.0 (m/emax normalized)))
      ;; Check specific values
      (is (= 0.0 (m/mget normalized 0 0))) ; min value
      (is (= 1.0 (m/mget normalized 2 2))) ; max value
      (is (= 0.5 (m/mget normalized 1 1))))) ; middle value

  (testing "Normalization of constant matrix"
    (let [matrix (m/matrix [[5 5]
                            [5 5]])
          normalized (matrix/normalize-matrix matrix)]

      ;; All values should remain the same for constant matrix
      (is (= 5.0 (m/mget normalized 0 0)))
      (is (= 5.0 (m/mget normalized 1 1))))))


(deftest test-print-matrix
  (testing "Matrix printing doesn't throw errors"
    (let [matrix (m/matrix [[1.234 2.567]
                            [3.891 4.123]])
          labels ["A" "B"]]

      ;; Just test that it doesn't throw an exception
      (is (not (nil? (with-out-str (matrix/print-matrix matrix)))))
      (is (not (nil? (with-out-str (matrix/print-matrix matrix {:labels labels})))))
      (is (not (nil? (with-out-str (matrix/print-matrix matrix {:precision 2}))))))))


(deftest test-matrix-conversion
  (testing "Matrix to nested vector conversion"
    (let [matrix (m/matrix [[1 2]
                            [3 4]])
          nested-vec (matrix/matrix->nested-vec matrix)]

      (is (= [[1.0 2.0] [3.0 4.0]] nested-vec))
      (is (vector? nested-vec))
      (is (vector? (first nested-vec))))))


(deftest test-edge-cases
  (testing "Empty and single-element cases"
    ;; Single element vector
    (let [v ["single"]
          result (matrix/symmetric (fn [_ _] 1) v)]
      (is (= 1 (m/row-count result)))
      (is (= 1 (m/column-count result))))

    ;; Empty vector should handle gracefully
    (let [v []
          result (matrix/symmetric (fn [_ _] 1) v)]
      (is (= 0 (m/row-count result)))
      (is (= 0 (m/column-count result))))))
