;; # MCL Clustering Visualisierung
;; 
;; Dieses Notebook visualisiert den Markov Clustering Algorithm Schritt für Schritt.

(ns initiae.mcl-visualization
  {:nextjournal.clerk/visibility {:code :hide}}
  (:require [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [initiae.core :as core]
            [initiae.text-metric :as metric]
            [initiae.matrix :as matrix]
            [initiae.mcl :as mcl]
            [initiae.character-costs :as costs]))

;; ## Daten laden

^{::clerk/visibility {:code :show :result :hide}}
(def initiae (core/load-initiae))

^{::clerk/visibility {:result :show}}
(clerk/table 
  {:head ["Index" "Initium"]
   :rows (map-indexed (fn [idx text] [idx text]) (take 10 initiae))})

(clerk/md (str "Insgesamt " (count initiae) " Initien geladen."))

;; ## Ähnlichkeitsmatrix berechnen

^{::clerk/visibility {:code :show}}
(def selected-metric :weighted-lev-sim)

^{::clerk/visibility {:result :hide}}
(def similarity-matrix
  (let [metric-fn (core/get-metric selected-metric)]
    (matrix/symmetric metric-fn initiae)))

;; ## Matrix-Heatmap Visualisierung

(defn matrix-heatmap
  "Erstellt eine Heatmap-Visualisierung einer Matrix."
  [matrix labels & {:keys [max-size] :or {max-size 50}}]
  (let [n (min (m/row-count matrix) max-size)
        data (for [i (range n)
                   j (range n)]
               {:x j
                :y i
                :value (m/mget matrix i j)
                :label-x (nth labels j)
                :label-y (nth labels i)})]
    (clerk/vl
      {:width 800
       :height 800
       :data {:values data}
       :mark "rect"
       :encoding {:x {:field "x"
                      :type "ordinal"
                      :axis {:title "Initium Index"
                             :labelAngle -45
                             :labelLimit 100}}
                  :y {:field "y"
                      :type "ordinal"
                      :axis {:title "Initium Index"}}
                  :color {:field "value"
                          :type "quantitative"
                          :scale {:scheme "viridis"
                                  :domain [0 1]}
                          :legend {:title "Ähnlichkeit"}}
                  :tooltip [{:field "label-x" :title "Initium X"}
                            {:field "label-y" :title "Initium Y"}
                            {:field "value" :title "Ähnlichkeit" :format ".3f"}]}})))

;; Visualisiere die gesamte Matrix (oder erste 50 bei großen Datensätzen)
^{::clerk/visibility {:code :hide}}
(let [display-size (min (count initiae) 50)
      sub-matrix (if (> (count initiae) display-size)
                   (m/submatrix similarity-matrix [[0 display-size] [0 display-size]])
                   similarity-matrix)
      sub-labels (take display-size initiae)]
  (clerk/col
    (clerk/md (str "### Ähnlichkeitsmatrix (" 
                   (if (> (count initiae) display-size)
                     (str "erste " display-size " von " (count initiae))
                     "alle")
                   " Initien)"))
    (matrix-heatmap sub-matrix (map #(str (subs % 0 (min 30 (count %)))) sub-labels))))

;; ## Netzwerk-Visualisierung

(defn create-network-data
  "Erstellt Netzwerk-Daten aus der Ähnlichkeitsmatrix."
  [matrix labels threshold]
  (let [n (m/row-count matrix)
        nodes (map (fn [i label] 
                    {:id i 
                     :label (subs label 0 (min 40 (count label)))
                     :group 0})
                  (range n) labels)
        edges (for [i (range n)
                    j (range (inc i) n)
                    :let [weight (m/mget matrix i j)]
                    :when (> weight threshold)]
                {:source i
                 :target j
                 :weight weight
                 :thickness (* 5 weight)})]
    {:nodes nodes :edges edges}))

(defn network-viz
  "Erstellt eine Force-Directed Graph Visualisierung."
  [network-data]
  (clerk/vl
    {:width 900
     :height 700
     :data [{:name "node-data"
             :values (:nodes network-data)}
            {:name "link-data"
             :values (:edges network-data)}]
     :scales [{:name "color"
               :type "ordinal"
               :domain {:data "node-data" :field "group"}
               :range {:scheme "category20"}}]
     :marks [{:name "nodes"
              :type "symbol"
              :from {:data "node-data"}
              :encode {:enter {:fill {:scale "color" :field "group"}
                              :stroke {:value "white"}
                              :strokeWidth {:value 2}}
                      :update {:size {:value 300}
                              :cursor {:value "pointer"}}
                      :hover {:size {:value 500}}}}
             {:name "links"
              :type "path"
              :from {:data "link-data"}
              :interactive false
              :encode {:enter {:stroke {:value "#ccc"}
                              :strokeWidth {:field "thickness"}}
                      :update {:stroke {:value "#ccc"}
                              :strokeOpacity {:signal "0.3 + datum.weight * 0.7"}}}}
             {:type "text"
              :from {:data "node-data"}
              :encode {:enter {:align {:value "center"}
                              :baseline {:value "middle"}
                              :fontSize {:value 10}
                              :fontWeight {:value "bold"}
                              :fill {:value "black"}
                              :text {:field "label"}}}}]
     :signals [{:name "cx" :update "width / 2"}
               {:name "cy" :update "height / 2"}
               {:name "nodeRadius" :value 8}
               {:name "nodeCharge" :value -30}
               {:name "linkDistance" :value 50}]
     :forces [{:force "center" :x {:signal "cx"} :y {:signal "cy"}}
              {:force "collide" :radius {:signal "nodeRadius"}}
              {:force "nbody" :strength {:signal "nodeCharge"}}
              {:force "link" :links "link-data" :distance {:signal "linkDistance"}}]}))

;; ### Netzwerk mit Schwellenwert

^{::clerk/visibility {:code :show}}
(def threshold 0.7)  ; Erhöht für bessere Übersichtlichkeit

^{::clerk/visibility {:code :hide}}
(clerk/md (str "### Netzwerk-Visualisierung (Ähnlichkeit > " threshold ")"))

;; Verwende alle Initien für das Netzwerk
(let [network-data (create-network-data similarity-matrix initiae threshold)]
  (clerk/col
    (clerk/md (str "Knoten: " (count (:nodes network-data)) 
                   ", Kanten: " (count (:edges network-data))))
    (network-viz network-data)))

;; ## MCL Schritt-für-Schritt

^{::clerk/visibility {:code :show}}
(def mcl-params
  {:inflation 2.0
   :max-iterations 10
   :prune-threshold 0.01})

(defn mcl-step
  "Führt einen einzelnen MCL-Schritt aus."
  [matrix inflation prune-threshold]
  (-> matrix
      mcl/expand
      (mcl/inflate inflation)
      (mcl/prune-matrix prune-threshold)))

(defn mcl-iteration-data
  "Generiert Daten für eine MCL-Iteration."
  [matrix iteration]
  {:iteration iteration
   :matrix matrix
   :stats (matrix/matrix-stats matrix)})

;; ### MCL Iterationen visualisieren

^{::clerk/visibility {:code :hide}}
(def mcl-iterations
  (let [normalized (mcl/normalize-columns similarity-matrix)]
    (loop [matrix normalized
           iterations []
           iter 0]
      (if (>= iter 5) ; Zeige nur erste 5 Iterationen
        iterations
        (let [new-matrix (mcl-step matrix 
                                  (:inflation mcl-params) 
                                  (:prune-threshold mcl-params))]
          (recur new-matrix
                 (conj iterations (mcl-iteration-data matrix iter))
                 (inc iter)))))))

;; Visualisiere jede Iteration
(clerk/md "### MCL Iterationen")

(for [{:keys [iteration matrix stats]} mcl-iterations]
  (clerk/col
    (clerk/md (str "#### Iteration " iteration))
    (clerk/md (str "Min: " (format "%.4f" (:min stats)) 
                  ", Max: " (format "%.4f" (:max stats))
                  ", Mean: " (format "%.4f" (:mean stats))))
    (matrix-heatmap matrix (map str (range (m/row-count matrix))) :max-size 30)))

;; ## Interaktive Parameter-Kontrolle

(defn run-mcl-with-params
  "Führt MCL mit gegebenen Parametern aus."
  [inflation prune-threshold max-iterations]
  (let [result (mcl/mcl similarity-matrix 
                       :inflation inflation
                       :prune-threshold prune-threshold
                       :max-iterations max-iterations
                       :ensure-complete true)]
    {:result result
     :labels initiae
     :matrix (:matrix result)}))

;; ### Parameter-Experimente

^{::clerk/visibility {:code :show}}
(def inflation-values [1.2 1.5 2.0 2.5 3.0])

(def experiments
  (for [inf inflation-values]
    (let [{:keys [result labels matrix]} (run-mcl-with-params inf 0.01 50)]
      {:inflation inf
       :converged (:converged result)
       :iterations (:iterations result)
       :num-clusters (count (:clusters result))
       :cluster-sizes (sort (map count (:clusters result)))
       :matrix matrix})))

^{::clerk/visibility {:code :hide}}
(clerk/table
  {:head ["Inflation" "Konvergiert" "Iterationen" "Anzahl Cluster" "Cluster-Größen"]
   :rows (map (fn [exp]
               [(:inflation exp)
                (str (:converged exp))
                (:iterations exp)
                (:num-clusters exp)
                (str (:cluster-sizes exp))])
             experiments)})

;; ### Finale Cluster-Visualisierung

(defn visualize-clusters
  "Visualisiert die finalen Cluster als gefärbtes Netzwerk."
  [matrix labels clusters]
  (let [n (count labels)
        ;; Erstelle Cluster-Zuordnung
        node-to-cluster (reduce (fn [acc [cluster-id cluster]]
                                 (reduce #(assoc %1 %2 cluster-id) 
                                        acc cluster))
                               {}
                               (map-indexed vector clusters))
        ;; Erstelle Knoten mit Cluster-Farben
        nodes (map (fn [i label]
                    {:id i
                     :label (subs label 0 (min 40 (count label)))
                     :group (get node-to-cluster i 0)})
                  (range n) labels)
        ;; Erstelle Kanten basierend auf finaler Matrix
        edges (for [i (range n)
                    j (range (inc i) n)
                    :let [weight (m/mget matrix i j)]
                    :when (> weight 0.01)]
                {:source i
                 :target j
                 :weight weight
                 :thickness (* 10 weight)})]
    (network-viz {:nodes nodes :edges edges})))

;; Zeige finale Cluster mit eingeschränkter Visualisierung für große Datensätze
^{::clerk/visibility {:code :hide}}
(let [{:keys [result labels matrix]} (run-mcl-with-params 2.0 0.01 50)
      cluster-indices (:clusters result)
      ;; Für große Netzwerke, zeige nur eine Teilmenge
      max-nodes 100
      display-all? (<= (count labels) max-nodes)]
  (clerk/col
    (clerk/md "### Finale Cluster-Zuordnung")
    (clerk/md (str "Gefunden: " (count cluster-indices) " Cluster"))
    (if display-all?
      (visualize-clusters matrix labels cluster-indices)
      (let [;; Nehme repräsentative Knoten aus jedem Cluster
            sample-indices (take max-nodes 
                               (distinct 
                                 (mapcat #(take (max 1 (quot max-nodes (count cluster-indices))) %) 
                                        cluster-indices)))
            sample-matrix (m/compute-matrix [(count sample-indices) (count sample-indices)]
                                          (fn [i j] 
                                            (m/mget matrix 
                                                   (nth sample-indices i) 
                                                   (nth sample-indices j))))
            sample-labels (map #(nth labels %) sample-indices)
            ;; Erstelle neue Cluster-Indizes für die Teilmenge
            index-map (into {} (map-indexed (fn [i idx] [idx i]) sample-indices))
            sample-clusters (for [cluster cluster-indices]
                             (keep #(index-map %) cluster))]
        (clerk/col
          (clerk/md (str "**Hinweis:** Zeige nur " (count sample-indices) 
                        " von " (count labels) " Knoten für bessere Übersichtlichkeit"))
          (visualize-clusters sample-matrix sample-labels sample-clusters))))))

;; ## Cluster-Inhalte

^{::clerk/visibility {:code :hide}}
(let [{:keys [result labels]} (run-mcl-with-params 2.0 0.01 50)
      labeled-clusters (for [cluster (:clusters result)]
                        (mapv #(nth labels %) cluster))]
  (clerk/col
    (clerk/md "### Cluster-Details")
    (for [[idx cluster] (map-indexed vector labeled-clusters)]
      (clerk/col
        (clerk/md (str "**Cluster " (inc idx) " (" (count cluster) " Initien):**"))
        (clerk/code (str/join "\n" (map #(str "  - " %) cluster)))))))

;; ## Export-Funktionen

^{::clerk/visibility {:code :show}}
(comment
  ;; Speichere Visualisierung als HTML
  (clerk/build! {:paths ["src/initiae/mcl_visualization.clj"]
                 :out-path "docs/mcl-visualization.html"})
  
  ;; Starte Clerk Server
  (clerk/serve! {:watch-paths ["src/initiae"]}))
