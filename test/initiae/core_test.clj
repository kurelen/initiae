(ns initiae.core-test
  (:require
    [clojure.core.matrix :as m]
    [clojure.set :refer [subset?]]
    [clojure.test :refer [deftest is testing]]
    [initiae.core :as core]))


;; Test data
(def test-initiae
  ["Ave maria gratia plena"
   "Ave maria gracia plena"  ; similar to first
   "Pange lingua gloriosi"   ; different
   "Ave sancta maria"])      ; somewhat similar to first


(deftest test-available-metrics
  (testing "All metrics are properly defined"
    (is (map? core/available-metrics))
    (is (< 10 (count core/available-metrics)))

    ;; Check that each metric has name and function
    (doseq [[k [name fn]] core/available-metrics]
      (is (keyword? k))
      (is (string? name))
      (is (fn? fn)))))


(deftest test-get-metric
  (testing "Valid metrics return functions"
    (let [metric-fn (core/get-metric :levenshtein-sim)]
      (is (fn? metric-fn))
      (is (number? (metric-fn "test" "test")))))

  (testing "Invalid metric throws exception"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"Unknown metric"
          (core/get-metric :invalid-metric)))))


(deftest test-analyze-similarities
  (testing "Returns proper analysis structure"
    (let [result (core/analyze-similarities test-initiae :levenshtein-sim)]
      (is (= (:metric result) :levenshtein-sim))
      (is (string? (:metric-name result)))
      (is (= (:initiae result) test-initiae))
      (is (m/matrix? (:matrix result)))
      (is (= (m/shape (:matrix result)) [4 4]))))

  (testing "Matrix is symmetric for similarity metrics"
    (let [result (core/analyze-similarities test-initiae :levenshtein-sim)
          matrix (:matrix result)]
      (is (m/equals matrix (m/transpose matrix) 1e-10))))

  (testing "Diagonal elements are 1.0 for similarity metrics"
    (let [result (core/analyze-similarities test-initiae :levenshtein-sim)
          matrix (:matrix result)]
      (dotimes [i 4]
        (is (= (m/mget matrix i i) 1.0))))))


(deftest test-cluster-initiae
  (testing "Returns clustering result with required keys"
    (let [result (core/cluster-initiae test-initiae)]
      (is (contains? result :metric))
      (is (contains? result :metric-name))
      (is (contains? result :matrix))
      (is (contains? result :initiae))
      (is (contains? result :converged))
      (is (contains? result :iterations))
      (is (contains? result :clusters))
      (is (contains? result :labeled-clusters))))

  (testing "Custom parameters are respected"
    (let [result (core/cluster-initiae test-initiae
                                       :metric :jaro-winkler-sim
                                       :inflation 1.5)]
      (is (= (:metric result) :jaro-winkler-sim))
      (is (boolean? (:converged result)))
      (is (pos-int? (:iterations result)))))

  (testing "Labeled clusters contain original strings"
    (let [result (core/cluster-initiae test-initiae)
          all-clustered (apply concat (:labeled-clusters result))]
      (is (subset? (set all-clustered) (set test-initiae))))))


(deftest test-validate-args
  (testing "Help flag returns help message"
    (let [result (core/validate-args ["--help"])]
      (is (contains? result :exit-message))
      (is (:ok? result))
      (is (re-find #"Medieval Incipit Clustering Tool" (:exit-message result))))))


(testing "Valid arguments return options"
  (let [result (core/validate-args ["--metric" "levenshtein-sim" "--inflation" "1.5"])]
    (is (= (:action result) :run))
    (is (= (get-in result [:options :metric]) :levenshtein-sim))
    (is (= (get-in result [:options :inflation]) 1.5))))


(testing "Invalid metric returns error"
  (let [result (core/validate-args ["--metric" "invalid-metric"])]
    (is (contains? result :exit-message))
    (is (false? (:ok? result)))))


(testing "Invalid inflation value returns error"
  (let [result (core/validate-args ["--inflation" "not-a-number"])]
    (is (contains? result :exit-message))
    (is (false? (:ok? result)))))


(testing "Default values are set correctly"
  (let [result (core/validate-args [])]
    (is (= (:action result) :run))
    (is (= (get-in result [:options :metric]) :weighted-lev-sim))
    (is (= (get-in result [:options :inflation]) 2.0))
    (is (= (get-in result [:options :tolerance]) 1e-6))
    (is (= (get-in result [:options :max-iterations]) 2))))


(deftest test-metric-functions-work
  (testing "All available metrics can compute similarities"
    (doseq [[metric-key _] core/available-metrics]
      (testing (str "Metric: " metric-key)
        (let [metric-fn (core/get-metric metric-key)
              result (metric-fn "test" "test")]
          (is (number? result))
          (is (not (Double/isNaN result)))))))

  (testing "Similarity metrics return higher values for identical strings"
    (let [sim-metrics [:levenshtein-sim :jaro-winkler-sim :cosine-sim
                       :jaccard-sim :lcs-sim :ngram-sim :weighted-lev-sim]]
      (doseq [metric sim-metrics]
        (let [metric-fn (core/get-metric metric)
              identical-score (metric-fn "test" "test")
              different-score (metric-fn "test" "completely different")]
          (is (>= identical-score different-score)
              (str "Metric " metric " should give higher scores for identical strings"))))))

  (testing "Distance metrics return lower values for identical strings"
    (let [dist-metrics [:levenshtein-dist :jaro-winkler-dist :cosine-dist
                        :jaccard-dist :lcs-dist :damerau-dist :weighted-lev-dist]]
      (doseq [metric dist-metrics]
        (let [metric-fn (core/get-metric metric)
              identical-score (metric-fn "test" "test")
              different-score (metric-fn "test" "completely different")]
          (is (<= identical-score different-score)
              (str "Metric " metric " should give lower scores for identical strings")))))))


(deftest test-clustering-edge-cases
  (testing "Single incipit clustering"
    (let [result (core/cluster-initiae ["single item"])]
      (is (= (count (:labeled-clusters result)) 1))
      (is (= (first (:labeled-clusters result)) ["single item"]))))

  (testing "Empty initiae list"
    (is (not (nil? (core/cluster-initiae [])))))

  (testing "Identical initiae cluster together"
    (let [identical-initiae ["same text" "same text" "same text"]
          result (core/cluster-initiae identical-initiae)]
      ;; Should form one cluster
      (is (= (count (:labeled-clusters result)) 1))
      (is (= (first (:labeled-clusters result)) identical-initiae)))))


(comment
;; Integration test
(deftest test-end-to-end-clustering
  (testing "Complete clustering pipeline works"
    (let [result (core/cluster-initiae test-initiae 
                                       :metric :weighted-lev-sim
                                       :inflation 2.0)]
      ;; Verify structure
      (is (map? result))
      (is (:converged result))
      
      ;; Verify all initiae are clustered
      (let [all-clustered (apply concat (:labeled-clusters result))]
        (is (= (count all-clustered) (count test-initiae)))
        (is (= (set all-clustered) (set test-initiae))))
      
      ;; Similar strings should cluster together
      (let [clusters (:labeled-clusters result)
            ave-maria-variants ["Ave maria gratia plena" 
                               "Ave maria gracia plena" 
                               "Ave sancta maria"]]
        ;; At least some Ave Maria variants should be in the same cluster
        (is (some #(> (count (filter (set ave-maria-variants) %)) 1) clusters))))))

(deftest test-load-initiae
  (let [data (core/load-initiae)]
    (is (vector? data))
    (is (not (= 0 (count data))))))
)
