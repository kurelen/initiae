(ns initiae.mcl
  (:require
    [clojure.core.matrix :as m]
    [clojure.set :as set]))


(defn prune-matrix
  "Remove values below threshold to keep matrix sparse."
  [matrix threshold]
  (m/emap #(if (< % threshold) 0.0 %) matrix))


(defn normalize-columns
  "Normalize each column to sum to 1."
  [matrix]
  (let [n (m/row-count matrix)
        col-sums (for [j (range (m/column-count matrix))]
                   (reduce + (for [i (range n)]
                               (m/mget matrix i j))))]
    (m/div matrix (mapv #(max % 1e-10) col-sums)))) ; prevent division by zero

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
  (let [n (m/row-count matrix)
        m-cols (m/column-count matrix)
        diff-sum (reduce +
                         (for [i (range n)
                               j (range m-cols)]
                           (Math/abs (- (m/mget matrix i j)
                                        (m/mget prev-matrix i j)))))]
    (< diff-sum tolerance)))


(defn extract-clusters-complete
  "Extract clusters from converged MCL matrix, ensuring every node is assigned.
  
  This implementation:
  1. Finds attractors (nodes with strong self-loops)
  2. Assigns each node to its strongest attractor
  3. Creates singleton clusters for unassigned nodes"
  [matrix tolerance]
  (let [n (m/row-count matrix)
        ;; Find attractors (nodes with significant self-loops)
        attractors (set (for [j (range n)
                              :when (> (m/mget matrix j j) tolerance)]
                          j))
        ;; If no attractors found, make each node an attractor
        final-attractors (if (empty? attractors)
                           (set (range n))
                           attractors)
        ;; Assign each node to its strongest attractor
        node-assignments (for [i (range n)]
                           (let [attractor-values (for [a final-attractors]
                                                    [a (m/mget matrix i a)])
                                 max-val (apply max-key second attractor-values)]
                             [i (first max-val)]))
        ;; Group nodes by their assigned attractor
        clusters-map (reduce (fn [acc [node attractor]]
                               (update acc attractor (fnil conj []) node))
                             {}
                             node-assignments)]
    (vals clusters-map)))


(defn extract-clusters-threshold-based
  "Alternative clustering extraction based on thresholds.
  
  Parameters:
  - matrix: the converged MCL matrix
  - threshold: minimum value for considering a node as an attractor (high self-loop)
  - min-strength: minimum connection strength to consider nodes as connected
  
  This method:
  1. Identifies attractors (nodes with self-loops > threshold)
  2. For non-attractors, assigns them to clusters based on connections > min-strength
  3. Creates singleton clusters for any remaining unassigned nodes"
  [matrix threshold min-strength]
  (let [n (m/row-count matrix)
        ;; First identify strong attractors
        attractors (set (for [j (range n)
                              :when (> (m/mget matrix j j) threshold)]
                          j))
        ;; For each node, find its strongest connection to an attractor
        node-to-attractor (atom {})]
    ;; First, assign attractors to themselves
    (doseq [a attractors]
      (swap! node-to-attractor assoc a a))
    ;; Then assign other nodes to their strongest attractor
    (doseq [i (range n)
            :when (not (attractors i))]
      (let [attractor-connections (for [a attractors
                                        :let [strength (m/mget matrix i a)]
                                        :when (>= strength min-strength)]
                                    [a strength])]
        (when (seq attractor-connections)
          (let [[best-attractor _] (apply max-key second attractor-connections)]
            (swap! node-to-attractor assoc i best-attractor)))))
    ;; Group nodes by their attractor
    (let [clusters-map (reduce (fn [acc [node attractor]]
                                 (update acc attractor (fnil conj []) node))
                               {}
                               @node-to-attractor)
          ;; Find any unassigned nodes and make them singletons
          assigned-nodes (set (keys @node-to-attractor))
          unassigned (set/difference (set (range n)) assigned-nodes)
          singleton-clusters (map vector unassigned)]
      (concat (vals clusters-map) singleton-clusters))))


(defn mcl
  "Perform Markov Clustering on similarity matrix.
  
  Options:
  - :inflation - inflation parameter (default 2.0, higher = more clusters)
  - :max-iterations - maximum iterations (default 100)
  - :tolerance - convergence tolerance (default 1e-6)
  - :prune-threshold - threshold for pruning weak edges (default 1e-4)
  - :ensure-complete - ensure every node is assigned to a cluster (default true)"
  [similarity-matrix & {:keys [inflation max-iterations tolerance prune-threshold ensure-complete]
                        :or {inflation 2.0
                             max-iterations 100
                             tolerance 1e-6
                             prune-threshold 1e-4
                             ensure-complete true}}]
  (loop [matrix (normalize-columns similarity-matrix)
         prev-matrix nil
         iteration 0]
    (cond
      (<= max-iterations iteration)
      {:converged false
       :iterations iteration
       :clusters (if ensure-complete
                   (extract-clusters-complete matrix tolerance)
                   (extract-clusters-threshold-based matrix tolerance prune-threshold))
       :matrix matrix}

      (and prev-matrix (has-converged? matrix prev-matrix tolerance))
      {:converged true
       :iterations iteration
       :clusters (if ensure-complete
                   (extract-clusters-complete matrix tolerance)
                   (extract-clusters-threshold-based matrix tolerance prune-threshold))
       :matrix matrix}

      :else
      (let [expanded (expand matrix)
            inflated (inflate expanded inflation)
            pruned (if (> prune-threshold 0)
                     (prune-matrix inflated prune-threshold)
                     inflated)]
        (recur pruned matrix (inc iteration))))))


(defn cluster-with-labels
  "Cluster incipits and return results with original labels/indices."
  [incipits similarity-matrix & mcl-options]
  (let [result (apply mcl similarity-matrix mcl-options)
        clusters (:clusters result)
        ;; Verify all nodes are assigned
        all-nodes (set (apply concat clusters))
        expected-nodes (set (range (count incipits)))]
    (when-not (= all-nodes expected-nodes)
      (println "Warning: Not all incipits were assigned to clusters!"
               "\nMissing:" (set/difference expected-nodes all-nodes)))
    (assoc result
           :labeled-clusters
           (for [cluster clusters]
             (mapv #(nth incipits %) cluster)))))


;; Utility functions for analysis
(defn cluster-stats
  "Generate statistics about clustering results."
  [clusters]
  (let [all-nodes (set (apply concat clusters))
        expected-nodes (set (range (count all-nodes)))]
    {:num-clusters (count clusters)
     :cluster-sizes (mapv count clusters)
     :largest-cluster (apply max (map count clusters))
     :smallest-cluster (apply min (map count clusters))
     :singletons (count (filter #(= 1 (count %)) clusters))
     :total-nodes (count all-nodes)
     :missing-nodes (set/difference expected-nodes all-nodes)}))


(defn print-clusters
  "Pretty print clustering results."
  [labeled-clusters]
  (doseq [[i cluster] (map-indexed vector labeled-clusters)]
    (println (str "Cluster " (inc i) " (" (count cluster) " items):"))
    (doseq [item cluster]
      (println (str "  " item)))
    (println)))


;; Additional analysis functions

(defn analyze-convergence
  "Analyze the convergence behavior of MCL with different parameters."
  [similarity-matrix & {:keys [inflations]
                        :or {inflations [1.2 1.5 2.0 2.5 3.0]}}]
  (for [inf inflations]
    (let [result (mcl similarity-matrix :inflation inf)]
      {:inflation inf
       :converged (:converged result)
       :iterations (:iterations result)
       :num-clusters (count (:clusters result))
       :cluster-sizes (sort (map count (:clusters result)))})))


(defn find-optimal-inflation
  "Find inflation parameter that gives desired number of clusters."
  [similarity-matrix target-clusters & {:keys [min-inflation max-inflation steps]
                                        :or {min-inflation 1.1
                                             max-inflation 5.0
                                             steps 20}}]
  (let [inflations (map #(+ min-inflation (* % (/ (- max-inflation min-inflation) steps)))
                        (range (inc steps)))
        results (map (fn [inf]
                       (let [res (mcl similarity-matrix :inflation inf)]
                         {:inflation inf
                          :num-clusters (count (:clusters res))
                          :diff (Math/abs (- target-clusters (count (:clusters res))))}))
                     inflations)]
    (apply min-key :diff results)))
