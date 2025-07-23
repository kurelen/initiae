;; # MCL Clustering Visualisierung
;; 
;; Dieses Notebook visualisiert den Markov Clustering Algorithm Schritt für Schritt.

(ns notebooks.mcl-visualization
  {:nextjournal.clerk/visibility {:code :hide}}
  (:require
    [clojure.core.matrix :as m]
    [clojure.string :as str]
    [initiae.core :as core]
    [initiae.matrix :as matrix]
    [initiae.mcl :as mcl]
    [nextjournal.clerk :as clerk]))


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
        stats (matrix/matrix-stats matrix)
        domain [(:min stats) (:max stats)]

        data (for [i (range n)
                   j (range n)]
               {:x j
                :y i
                :value (m/mget matrix i j)
                :label-x (nth labels j)
                :label-y (nth labels i)})]
    (clerk/vl
      {:width 600
       :height 600
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
                          :scale {:scheme "magma"
                                  :domain domain}
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
    (matrix-heatmap sub-matrix (map #(subs % 0 (min 30 (count %))) sub-labels))))


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


(defn network-viz-simple
  "Erstellt eine einfachere Netzwerk-Visualisierung mit Vega-Lite."
  [network-data]
  ;; Berechne Layout mit einem einfachen Spring-Algorithmus
  (let [nodes (:nodes network-data)
        edges (:edges network-data)
        n (count nodes)
        ;; Initialisiere Positionen im Kreis
        positions (into {}
                        (map-indexed
                          (fn [i node]
                            [(:id node)
                             {:x (+ 450 (* 300 (Math/cos (* 2 Math/PI (/ i n)))))
                              :y (+ 350 (* 300 (Math/sin (* 2 Math/PI (/ i n)))))}])
                          nodes))
        ;; Erstelle erweiterte Knoten- und Kanten-Daten
        nodes-with-pos (map (fn [node]
                              (merge node (get positions (:id node))))
                            nodes)
        edges-with-pos (map (fn [edge]
                              (let [source-pos (get positions (:source edge))
                                    target-pos (get positions (:target edge))]
                                (assoc edge
                                       :x source-pos
                                       :y target-pos
                                       :x2 (:x target-pos)
                                       :y2 (:y target-pos))))
                            edges)]
    (clerk/vl
      {:width 900
       :height 700
       :layer [{;; Kanten-Layer
                :data {:values edges-with-pos}
                :mark {:type "rule"
                       :strokeWidth {:expr "datum.thickness"}
                       :opacity {:expr "0.3 + datum.weight * 0.5"}}
                :encoding {:x {:field "x" :type "quantitative" :scale {:domain [0 900]}}
                           :y {:field "y" :type "quantitative" :scale {:domain [0 700]}}
                           :x2 {:field "x2"}
                           :y2 {:field "y2"}
                           :stroke {:value "#999"}}}
               {;; Knoten-Layer
                :data {:values nodes-with-pos}
                :mark {:type "circle" :size 400}
                :encoding {:x {:field "x" :type "quantitative"}
                           :y {:field "y" :type "quantitative"}
                           :color {:field "group" :type "nominal" :scale {:scheme "tableau20"}}
                           :tooltip [{:field "label" :type "nominal"}
                                     {:field "id" :type "quantitative"}]}}
               {;; Text-Layer
                :data {:values nodes-with-pos}
                :mark {:type "text"
                       :dy -15
                       :fontSize 10}
                :encoding {:x {:field "x" :type "quantitative"}
                           :y {:field "y" :type "quantitative"}
                           :text {:field "label" :type "nominal"}}}]})))


(defn network-viz
  "Wrapper-Funktion die die einfachere Visualisierung verwendet."
  [network-data]
  (network-viz-simple network-data))


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
  {:inflation 2
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
    (matrix-heatmap matrix (map str (range (m/row-count matrix))) :max-size 50 :domain [(:min stats) (:max stats)])))


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
       :labels labels
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
