(ns initiae.mcl
  (:require [clojure.core.matrix :as m]))

(defn prune-matrix
  "Remove values below threshold to keep matrix sparse."
  [matrix threshold]
  (m/emap #(if (< % threshold) 0.0 %) matrix))

(defn normalize-columns
  "Normalize each column to sum to 1."
  [matrix]
  (let [col-sums (m/sum matrix 0)]
    (m/div matrix (m/max col-sums 1e-10)))) ; prevent division by zero

(defn inflate
  "Apply inflation step: element-wise power followed by column normalization."
  [matrix inflation-param]
  (-> matrix
      (m/pow inflation-param)
      normalize-columns))

(defn expand
  "Apply expansion step: matrix multiplication with itself."
  [matrix]
  (m/mmul matrix matrix))

(defn has-converged?
  "Check if matrix has converged by comparing with previous iteration."
  [matrix prev-matrix tolerance]
  (< (m/distance matrix prev-matrix) tolerance))

(defn extract-clusters
  "Extract clusters from converged MCL matrix."
  [matrix tolerance]
  (let [n (m/row-count matrix)
        attractors (for [j (range n)
                         :when (> (m/mget matrix j j) tolerance)]
                     j)]
    (for [attractor attractors]
      (for [i (range n)
            :when (> (m/mget matrix i attractor) tolerance)]
        i))))

(defn mcl
  "Perform Markov Clustering on similarity matrix.
  
  Options:
  - :inflation - inflation parameter (default 2.0, higher = more clusters)
  - :max-iterations - maximum iterations (default 100)
  - :tolerance - convergence tolerance (default 1e-6)
  - :prune-threshold - threshold for pruning weak edges (default 1e-4)"
  [similarity-matrix & {:keys [inflation max-iterations tolerance prune-threshold]
                        :or {inflation 2.0
                             max-iterations 100
                             tolerance 1e-6
                             prune-threshold 1e-4}}]
  (loop [matrix (normalize-columns similarity-matrix)
         prev-matrix nil
         iteration 0]
    (cond
      (>= iteration max-iterations)
      {:converged false
       :iterations iteration
       :clusters (extract-clusters matrix tolerance)}
      
      (and prev-matrix (has-converged? matrix prev-matrix tolerance))
      {:converged true
       :iterations iteration
       :clusters (extract-clusters matrix tolerance)}
      
      :else
      (let [expanded (expand matrix)
            inflated (inflate expanded inflation)
            pruned (prune-matrix inflated prune-threshold)]
        (recur pruned matrix (inc iteration))))))

(defn cluster-with-labels
  "Cluster incipits and return results with original labels/indices."
  [incipits similarity-matrix & mcl-options]
  (let [result (apply mcl similarity-matrix mcl-options)
        clusters (:clusters result)]
    (assoc result
           :labeled-clusters
           (for [cluster clusters]
             (mapv #(nth incipits %) cluster)))))

;; Utility functions for analysis
(defn cluster-stats
  "Generate statistics about clustering results."
  [clusters]
  {:num-clusters (count clusters)
   :cluster-sizes (mapv count clusters)
   :largest-cluster (apply max (map count clusters))
   :smallest-cluster (apply min (map count clusters))
   :singletons (count (filter #(= 1 (count %)) clusters))})

(defn print-clusters
  "Pretty print clustering results."
  [labeled-clusters]
  (doseq [[i cluster] (map-indexed vector labeled-clusters)]
    (println (str "Cluster " (inc i) " (" (count cluster) " items):"))
    (doseq [item cluster]
      (println (str "  " item)))
    (println)))
