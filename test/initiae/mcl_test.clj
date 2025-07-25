(ns initiae.mcl-test
  (:require
    [clojure.core.matrix :as m]
    [clojure.test :refer [deftest is testing]]
    [initiae.mcl :as mcl]))


(defn similiar?
  [a b]
  (< (Math/abs (- a b)) 1e-10))


(deftest test-prune-matrix
  (testing "Matrix pruning removes small values"
    (let [matrix (m/matrix [[0.8 0.1 0.05]
                            [0.1 0.7 0.02]
                            [0.1 0.2 0.93]])
          pruned (mcl/prune-matrix matrix 0.1)]

      (is (= 0.8 (m/mget pruned 0 0)))
      (is (= 0.1 (m/mget pruned 0 1)))
      (is (= 0.0 (m/mget pruned 0 2))) ; 0.05 -> 0.0
      (is (= 0.1 (m/mget pruned 1 0)))
      (is (= 0.7 (m/mget pruned 1 1)))
      (is (= 0.0 (m/mget pruned 1 2))) ; 0.02 -> 0.0
      (is (= 0.93 (m/mget pruned 2 2))))))


(deftest test-normalize-columns
  (testing "Column normalization"
    (let [matrix (m/matrix [[1 2]
                            [3 4]])
          normalized (mcl/normalize-columns matrix)]

      ;; Check that columns sum to approximately 1
      (let [col1-sum (+ (m/mget normalized 0 0) (m/mget normalized 1 0))
            col2-sum (+ (m/mget normalized 0 1) (m/mget normalized 1 1))]
        (is (similiar? col1-sum 1.0))
        (is (similiar? col2-sum 1.0)))

      ;; Check specific values
      (is (= 0.25 (m/mget normalized 0 0))) ; 1/4
      (is (= 0.75 (m/mget normalized 1 0))) ; 3/4
      (is (similiar? (/ 1 3) (m/mget normalized 0 1))) ; 2/6
      (is (similiar? (/ 2 3) (m/mget normalized 1 1))))) ; 4/6

  (testing "Normalization with zero columns"
    (let [matrix (m/matrix [[1 0]
                            [2 0]])
          normalized (mcl/normalize-columns matrix)]

      ;; Non-zero column should normalize properly
      (is (similiar? (/ 1 3) (m/mget normalized 0 0)))
      (is (similiar? (/ 2 3) (m/mget normalized 1 0)))

      ;; Zero column should remain zero (or very small)
      (is (< (m/mget normalized 0 1) 1e-9))
      (is (< (m/mget normalized 1 1) 1e-9)))))


(deftest test-inflate
  (testing "Matrix inflation"
    (let [matrix (m/matrix [[0.6 0.2]
                            [0.4 0.8]])
          inflated (mcl/inflate matrix 2.0)]

      ;; After squaring and normalizing
      ;; Column 1: [0.36, 0.16] -> [0.36/0.52, 0.16/0.52] = [0.692..., 0.307...]
      ;; Column 2: [0.04, 0.64] -> [0.04/0.68, 0.64/0.68] = [0.058..., 0.941...]
      (is (similiar? (m/mget inflated 0 0) (/ 0.36 0.52)))
      (is (similiar? (m/mget inflated 1 0) (/ 0.16 0.52)))
      (is (similiar? (m/mget inflated 0 1) (/ 0.04 0.68)))
      (is (similiar? (m/mget inflated 1 1) (/ 0.64 0.68))))))


(deftest test-expand
  (testing "Matrix expansion (squaring)"
    (let [matrix (m/matrix [[0.5 0.2]
                            [0.5 0.8]])
          expanded (mcl/expand matrix)]

      ;; Manual matrix multiplication
      ;; [0.5 0.2] * [0.5 0.2] = [0.35 0.26]
      ;; [0.5 0.8]   [0.5 0.8]   [0.65 0.74]
      (is (similiar? 0.35 (m/mget expanded 0 0))) ; 0.5*0.5 + 0.2*0.5
      (is (similiar? 0.26 (m/mget expanded 0 1))) ; 0.5*0.2 + 0.2*0.8
      (is (similiar? 0.65 (m/mget expanded 1 0))) ; 0.5*0.5 + 0.8*0.5
      (is (similiar? 0.74 (m/mget expanded 1 1)))))) ; 0.5*0.2 + 0.8*0.8

(deftest test-has-converged
  (testing "Convergence detection"
    (let [matrix1 (m/matrix [[0.5 0.2]
                             [0.5 0.8]])
          matrix2 (m/matrix [[0.501 0.199]
                             [0.499 0.801]])]

      ;; Should converge with loose tolerance
      (is (true? (mcl/has-converged? matrix1 matrix2 0.01)))

      ;; Should not converge with tight tolerance
      (is (false? (mcl/has-converged? matrix1 matrix2 0.0001))))))


(deftest test-extract-clusters-complete
  (testing "Complete cluster extraction from converged matrix"
    (let [;; A converged matrix with clear attractors
          matrix (m/matrix [[0.9 0.0 0.1]
                            [0.1 0.0 0.0]
                            [0.0 1.0 0.9]])
          clusters (mcl/extract-clusters-complete matrix 0.05)]

      ;; Should assign all nodes
      (let [all-nodes (set (apply concat clusters))]
        (is (= #{0 1 2} all-nodes)))

      ;; Should have at least one cluster
      (is (>= (count clusters) 1))
      (is (<= (count clusters) 3)))))


(deftest test-extract-clusters-threshold-based
  (testing "Threshold-based cluster extraction"
    (let [matrix (m/matrix [[0.9 0.0 0.1]
                            [0.0 0.9 0.0]
                            [0.1 0.0 0.9]])
          clusters (mcl/extract-clusters-threshold-based matrix 0.05 0.05)]

      ;; Should assign all nodes
      (let [all-nodes (set (apply concat clusters))]
        (is (= #{0 1 2} all-nodes)))

      ;; With this matrix structure, we expect separate clusters
      (is (>= (count clusters) 2)))))


(deftest test-mcl-simple-case
  (testing "MCL on a simple similarity matrix"
    (let [;; Simple 3x3 similarity matrix with two clear clusters
          similarity-matrix (m/matrix [[1.0 0.8 0.1]
                                       [0.8 1.0 0.1]
                                       [0.1 0.1 1.0]])
          result (mcl/mcl similarity-matrix :inflation 2.0 :max-iterations 50)]

      (is (map? result))
      (is (contains? result :converged))
      (is (contains? result :iterations))
      (is (contains? result :clusters))
      (is (contains? result :matrix))
      (is (boolean? (:converged result)))
      (is (number? (:iterations result)))
      (is (sequential? (:clusters result)))

      ;; Should find some clusters
      (is (> (count (:clusters result)) 0))
      (is (<= (count (:clusters result)) 3)) ; At most as many clusters as nodes

      ;; All nodes should be assigned
      (let [all-nodes (set (apply concat (:clusters result)))]
        (is (= #{0 1 2} all-nodes))))))


(deftest test-cluster-with-labels
  (testing "Clustering with labels"
    (let [items ["item1" "item2" "item3"]
          similarity-matrix (m/matrix [[1.0 0.9 0.1]
                                       [0.9 1.0 0.1]
                                       [0.1 0.1 1.0]])
          result (mcl/cluster-with-labels items similarity-matrix :inflation 2.0)]

      (is (contains? result :labeled-clusters))
      (is (sequential? (:labeled-clusters result)))

      ;; Each cluster should contain actual items, not indices
      (let [all-items (flatten (:labeled-clusters result))]
        (is (every? string? all-items))
        (is (every? #(contains? (set items) %) all-items))
        ;; All items should be present
        (is (= (set items) (set all-items)))))))


(deftest test-cluster-stats
  (testing "Cluster statistics generation"
    (let [clusters [[0 1] [2] [3 4 5]]
          stats (mcl/cluster-stats clusters)]

      (is (= 3 (:num-clusters stats)))
      (is (= [2 1 3] (:cluster-sizes stats)))
      (is (= 3 (:largest-cluster stats)))
      (is (= 1 (:smallest-cluster stats)))
      (is (= 1 (:singletons stats)))
      (is (= 6 (:total-nodes stats)))
      (is (empty? (:missing-nodes stats))))))


(deftest test-mcl-edge-cases
  (testing "MCL with edge cases"
    ;; Single node
    (let [matrix (m/matrix [[1.0]])
          result (mcl/mcl matrix)]
      (is (= 1 (count (:clusters result))))
      (is (= #{0} (set (apply concat (:clusters result))))))

    ;; Identity matrix (no connections)
    (let [matrix (m/matrix [[1.0 0.0 0.0]
                            [0.0 1.0 0.0]
                            [0.0 0.0 1.0]])
          result (mcl/mcl matrix :inflation 2.0)]
      (is (>= (count (:clusters result)) 1))
      (is (<= (count (:clusters result)) 3))
      ;; All nodes should be assigned
      (let [all-nodes (set (apply concat (:clusters result)))]
        (is (= #{0 1 2} all-nodes))))))


(deftest test-mcl-parameters
  (testing "MCL with different parameters"
    (let [similarity-matrix (m/matrix [[1.0 0.5 0.1]
                                       [0.5 1.0 0.1]
                                       [0.1 0.1 1.0]])
          result-low (mcl/mcl similarity-matrix :inflation 1.5)
          result-high (mcl/mcl similarity-matrix :inflation 4.0)]

      ;; Higher inflation should generally produce more clusters
      (is (<= (count (:clusters result-low))
              (count (:clusters result-high))))

      ;; Both should converge with reasonable iterations
      (is (<= (:iterations result-low) 100))
      (is (<= (:iterations result-high) 100))

      ;; All nodes should be assigned in both cases
      (let [all-nodes-low (set (apply concat (:clusters result-low)))
            all-nodes-high (set (apply concat (:clusters result-high)))]
        (is (= #{0 1 2} all-nodes-low))
        (is (= #{0 1 2} all-nodes-high))))))


(deftest test-print-clusters
  (testing "Cluster printing doesn't throw errors"
    (let [clusters [["item1" "item2"] ["item3"]]]
      ;; Just test that it doesn't throw an exception
      (is (not (nil? (with-out-str (mcl/print-clusters clusters))))))))


(deftest test-ensure-complete-parameter
  (testing "ensure-complete parameter works correctly"
    (let [similarity-matrix (m/matrix [[1.0 0.1 0.0]
                                       [0.1 1.0 0.0]
                                       [0.0 0.0 1.0]])
          result-complete (mcl/mcl similarity-matrix
                                   :inflation 2.0
                                   :ensure-complete true)
          result-incomplete (mcl/mcl similarity-matrix
                                     :inflation 2.0
                                     :ensure-complete false)]

      ;; With ensure-complete=true, all nodes should be assigned
      (let [all-nodes (set (apply concat (:clusters result-complete)))]
        (is (= #{0 1 2} all-nodes)))

      ;; Both should produce valid results
      (is (> (count (:clusters result-complete)) 0))
      (is (> (count (:clusters result-incomplete)) 0)))))


(deftest test-analyze-convergence
  (testing "Convergence analysis function"
    (let [matrix (m/matrix [[1.0 0.8 0.1]
                            [0.8 1.0 0.1]
                            [0.1 0.1 1.0]])
          results (mcl/analyze-convergence matrix :inflations [1.5 2.0 2.5])]

      (is (= 3 (count results)))
      (is (every? #(contains? % :inflation) results))
      (is (every? #(contains? % :converged) results))
      (is (every? #(contains? % :num-clusters) results))
      (is (every? #(contains? % :cluster-sizes) results)))))


(deftest test-find-optimal-inflation
  (testing "Finding optimal inflation parameter"
    (let [matrix (m/matrix [[1.0 0.8 0.1]
                            [0.8 1.0 0.1]
                            [0.1 0.1 1.0]])
          result (mcl/find-optimal-inflation matrix 2
                                             :min-inflation 1.5
                                             :max-inflation 3.0
                                             :steps 5)]

      (is (contains? result :inflation))
      (is (contains? result :num-clusters))
      (is (contains? result :diff))
      (is (number? (:inflation result)))
      (is (<= 1.5 (:inflation result) 3.0)))))
