^{:nextjournal.clerk/visibility {:code :hide}}
(ns clustering-analysis
  (:require [nextjournal.clerk :as clerk]
            [initiae.matrix :as matrix]
            [initiae.character-costs :as costs]
            [initiae.text-metric :as metric]
            [initiae.mcl :as mcl]
            [clojure.core.matrix :as m]))

;; # Medieval Incipit Clustering Analysis
;; 
;; This notebook demonstrates clustering of medieval manuscript incipits using
;; weighted Levenshtein distance and Markov Clustering.

;; ## Sample Data
;; 
;; Here are some example incipits from medieval manuscripts:

(def sample-incipits
  ["Ave maria gratia plena"
   "Ave maria gracia plena"
   "Ave maria gracia plena dominus"
   "Pange lingua gloriosi"
   "Pangue lingua gloriosj"
   "Veni creator spiritus"
   "Veni creator spiritus mentes"
   "Salve regina misericordiae"
   "Salue regina misericordie"
   "Te deum laudamus"
   "Te deum laudamus te"])

^{:nextjournal.clerk/visibility {:result :hide}}
(clerk/table
  {:head ["Index" "Incipit"]
   :rows (map-indexed vector sample-incipits)})

;; ## Character Substitution Costs
;; 
;; We use paleographically-informed character substitution costs for medieval texts:

^{:nextjournal.clerk/visibility {:code :show}}
(def character-costs
  {:substitute
   {["a" "o"] 0.3
    ["a" "e"] 0.3
    ["ä" "e"] 0.0
    ["b" "p"] 0.0
    ["c" "g"] 0.3
    ["f" "v"] 0.0
    ;; ... (showing subset for brevity)
    }})

;; ## Similarity Matrix Generation
;; 
;; We create a similarity matrix using weighted Levenshtein distance:

^{:nextjournal.clerk/visibility {:code :show}}
(def similarity-fn
  (metric/weighted-levenshtein-sim-fn costs/paleographic-costs))

(def similarity-matrix
  (matrix/symmetric similarity-fn sample-incipits))

;; ### Similarity Matrix Visualization

^{:nextjournal.clerk/visibility {:code :hide}}
(defn format-matrix [matrix]
  (let [n (m/row-count matrix)]
    {:head (concat [""] (map str (range n)))
     :rows (for [i (range n)]
             (concat [i] 
                     (for [j (range n)]
                       (format "%.3f" (m/mget matrix i j)))))}))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (format-matrix similarity-matrix))

;; ## Clustering Analysis
;; 
;; We apply Markov Clustering with different inflation parameters:

^{:nextjournal.clerk/visibility {:code :show}}
(defn analyze-clustering [incipits inflation]
  (let [result (mcl/cluster-with-labels incipits similarity-matrix :inflation inflation)]
    {:inflation inflation
     :converged (:converged result)
     :iterations (:iterations result)
     :num-clusters (count (:clusters result))
     :cluster-sizes (mapv count (:clusters result))
     :labeled-clusters (:labeled-clusters result)}))

;; ### Results with Different Inflation Parameters

^{:nextjournal.clerk/visibility {:code :hide}}
(def clustering-results
  (mapv #(analyze-clustering sample-incipits %) [1.5 2.0 2.5 3.0 3.5]))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/table
  {:head ["Inflation" "Converged?" "Iterations" "Clusters" "Sizes"]
   :rows (for [result clustering-results]
           [(:inflation result)
            (:converged result)
            (:iterations result)
            (:num-clusters result)
            (clojure.string/join ", " (:cluster-sizes result))])})

;; ## Detailed Clustering Results
;; 
;; Let's examine the clustering with inflation = 2.0 in detail:

^{:nextjournal.clerk/visibility {:code :show}}
(def detailed-result (analyze-clustering sample-incipits 2.0))

^{:nextjournal.clerk/visibility {:code :hide}}
(defn format-clusters [labeled-clusters]
  (for [[i cluster] (map-indexed vector labeled-clusters)]
    {:cluster (str "Cluster " (inc i))
     :size (count cluster)
     :members (clojure.string/join "; " cluster)}))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/table
  {:head ["Cluster" "Size" "Members"]
   :rows (map (juxt :cluster :size :members) 
              (format-clusters (:labeled-clusters detailed-result)))})

;; ## Similarity Heatmap
;; 
;; Visual representation of the similarity matrix:

^{:nextjournal.clerk/visibility {:code :hide}}
(defn similarity-heatmap [matrix labels]
  (let [n (m/row-count matrix)]
    {:data (for [i (range n)
                 j (range n)]
             {:x j :y i :similarity (m/mget matrix i j)})
     :labels labels}))

^{:nextjournal.clerk/visibility {:result :show}}
(let [heatmap-data (similarity-heatmap similarity-matrix sample-incipits)]
  (clerk/vl
    {:data {:values (:data heatmap-data)}
     :mark {:type "rect" :stroke "white" :strokeWidth 1}
     :encoding {:x {:field "x" :type "ordinal" :title "Incipit Index"}
                :y {:field "y" :type "ordinal" :title "Incipit Index"}
                :color {:field "similarity" 
                        :type "quantitative"
                        :scale {:scheme "viridis"}
                        :legend {:title "Similarity"}}
                :tooltip [{:field "similarity" :type "quantitative" :format ".3f"}]}
     :width 400
     :height 400}))

;; ## Cluster Stability Analysis
;; 
;; How do cluster assignments change with different inflation parameters?

^{:nextjournal.clerk/visibility {:code :hide}}
(defn stability-analysis [results]
  (let [inflations (mapv :inflation results)
        cluster-counts (mapv :num-clusters results)]
    {:inflation-vs-clusters
     {:data (mapv (fn [inf clusters] {:inflation inf :clusters clusters})
                  inflations cluster-counts)}}))

^{:nextjournal.clerk/visibility {:result :show}}
(let [stability (stability-analysis clustering-results)]
  (clerk/vl
    {:data {:values (get-in stability [:inflation-vs-clusters :data])}
     :mark {:type "line" :point true :strokeWidth 2}
     :encoding {:x {:field "inflation" :type "quantitative" :title "Inflation Parameter"}
                :y {:field "clusters" :type "quantitative" :title "Number of Clusters"}}
     :width 400
     :height 300}))

;; ## Conclusions
;; 
;; - **Optimal inflation**: Around 2.0-2.5 seems to produce meaningful clusters
;; - **Paleographic weights**: The character substitution costs help group orthographic variants
;; - **Cluster quality**: Similar incipits (e.g., "Ave maria" variants) cluster together
;; - **Scalability**: The algorithm converges quickly for manuscript-sized datasets
;; 
;; This approach successfully identifies groups of related incipits that would be
;; valuable for manuscript studies and paleographic analysis.
