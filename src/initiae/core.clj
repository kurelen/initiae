(ns initiae.core
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [initiae.character-costs :as costs]
    [initiae.matrix :as matrix]
    [initiae.mcl :as mcl]
    [initiae.text-metric :as metric]))


;; Data Loading Functions

(defn load-initiae
  "Load initiae from a simple text file, one per line."
  []
  (with-open [r (io/reader (io/resource "fixtures/initiae.txt"))]
    (vec (line-seq r))))


;; Analysis Functions

(def available-metrics
  "Available similarity/distance metrics for analysis."
  {:ngram-sim            ["N-Gram similarity" metric/ngram-sim]
   :lcs-dist             ["LCS distance" metric/lcs-dist]
   :lcs-sim              ["LCS similarity" metric/lcs-sim]
   :cosine-dist          ["Cosine distance" metric/cosine-dist]
   :cosine-sim           ["Cosine similarity" metric/cosine-sim]
   :jaccard-dist         ["Jaccard distance" metric/jaccard-dist]
   :jaccard-sim          ["Jaccard similarity" metric/jaccard-sim]
   :jaro-winkler-dist    ["Jaro-Winkler distance" metric/jaro-winkler-dist]
   :jaro-winkler-sim     ["Jaro-Winkler similarity" metric/jaro-winkler-sim]
   :levenshtein-dist     ["Levenshtein distance" metric/levenshtein-dist]
   :levenshtein-sim      ["Levenshtein similarity" metric/levenshtein-sim]
   :damerau-dist         ["Damerau distance" metric/damerau-dist]
   :damerau-sim          ["Damerau similarity" metric/damerau-sim]
   :weighted-lev-dist    ["Weighted Levenshtein distance"
                          (metric/weighted-levenshtein-dist-fn costs/paleographic-costs)]
   :weighted-lev-sim     ["Weighted Levenshtein similarity"
                          (metric/weighted-levenshtein-sim-fn costs/paleographic-costs)]})


(defn get-metric
  "Get a metric function by keyword."
  [metric-key]
  (if-let [[_name metric-fn] (get available-metrics metric-key)]
    metric-fn
    (throw (ex-info "Unknown metric" {:metric metric-key
                                      :available (keys available-metrics)}))))


(defn analyze-similarities
  "Generate similarity matrix using specified metric."
  [initiae metric-key]
  (let [metric-fn (get-metric metric-key)]
    {:metric metric-key
     :metric-name (first (get available-metrics metric-key))
     :matrix (matrix/symmetric metric-fn initiae)
     :initiae initiae}))


(defn cluster-initiae
  "Perform MCL clustering on initiae using specified metric."
  [initiae & {:keys [metric inflation max-iterations tolerance]
              :or {metric :weighted-lev-sim
                   inflation 2.0
                   max-iterations 100
                   tolerance 1e-6}}]
  (let [analysis (analyze-similarities initiae metric)
        clustering-result (mcl/cluster-with-labels
                            initiae
                            (:matrix analysis)
                            :inflation inflation
                            :max-iterations max-iterations
                            :tolerance tolerance)]
    (merge analysis clustering-result)))


;; Output Functions

(defn print-matrix
  "Print a matrix with row labels for debugging."
  [matrix labels]
  (matrix/print-matrix matrix {:labels labels}))


(defn print-clustering-results
  "Pretty print clustering results."
  [result]
  (let [{:keys [metric-name converged iterations labeled-clusters]} result]
    (println (str "=== Clustering Results (" metric-name ") ==="))
    (println (str "Converged: " converged ", Iterations: " iterations))
    (println (str "Found " (count labeled-clusters) " clusters:"))
    (println)
    (mcl/print-clusters labeled-clusters)
    (println "=== Cluster Statistics ===")
    (pprint (mcl/cluster-stats (:clusters result)))))


(defn export-results
  "Export clustering results to EDN file."
  [result filename]
  (spit filename (pr-str result))
  (println (str "Results exported to " filename)))


;; CLI Options

(def cli-options
  [["-m" "--metric METRIC" "Similarity metric to use"
    :default :weighted-lev-sim
    :parse-fn keyword
    :validate [#(contains? available-metrics %)
               (str "Must be one of " (keys available-metrics))]]
   ["-i" "--inflation FLOAT" "MCL inflation parameter"
    :default 2.0
    :parse-fn #(Double/parseDouble %)]
   ["-t" "--tolerance FLOAT" "Convergence tolerance"
    :default 1e-6
    :parse-fn #(Double/parseDouble %)]
   ["-r" "--max-iterations INT" "Maximum iterations for MCL"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-o" "--output FILE" "Output file for results"]
   ["-v" "--verbose" "Verbose output"]
   ["-h" "--help" "Show help"]])


(defn usage
  [options-summary]
  (->> ["Medieval Incipit Clustering Tool"
        ""
        "Usage: clj -M:run [options]"
        "   or: java -jar initiae.jar [options]"
        ""
        "Options:"
        options-summary
        ""
        "Available metrics:"
        (str/join "\n" (for [[k [metric-name _]] available-metrics]
                         (str "  " (name k) " - " metric-name)))]
       (str/join \newline)))


(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))


(defn validate-args
  "Validate command line arguments."
  [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors) :ok? false}

      :else ; successful parse => return options
      {:action :run :options options})))


;; Main Function

(defn run-analysis
  "Run the complete clustering analysis."
  [options]
  (try
    (println "Loading initiae data...")
    (let [initiae (load-initiae)]
      (println (str "Loaded " (count initiae) " initiae"))

      (when (:verbose options)
        (println "Sample initiae:")
        (doseq [incipit (take 5 initiae)]
          (println (str "  " incipit)))
        (println))

      (println (str "Running clustering with "
                    (name (:metric options))
                    " (inflation=" (:inflation options) ")..."))

      (let [result (cluster-initiae initiae
                                    :metric (:metric options)
                                    :inflation (:inflation options)
                                    :max-iterations (:max-iterations options)
                                    :tolerance (:tolerance options))]
        (print-clustering-results result)

        (when-let [output-file (:output options)]
          (export-results result output-file))

        result))
    (catch Exception e
      (println (str "Error: " (.getMessage e)))
      (when (:verbose options)
        (.printStackTrace e))
      (System/exit 1))))


(defn -main
  "Main entry point for the application."
  [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (do (println exit-message)
          (System/exit (if ok? 0 1)))
      (case action
        :run (run-analysis options)
        (do (println "Unknown action")
            (System/exit 1))))))
