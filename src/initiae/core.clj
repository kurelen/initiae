(ns initiae.core
  (:gen-class)
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [join]]
    [initiae.matrix :as matrix]
    [initiae.text-metric :as metric]))


(defn load-fixture
  []
  (with-open
    [r (io/reader
         (io/resource "fixtures/initia_variants.edn"))]
    (edn/read (java.io.PushbackReader. r))))


(defn flatten-fixture
  [data]
  (->> data
       (mapcat #(reduce into [] (vals %)))))


(defn print-matrix
  [v matrix]
  (doseq [row (map #(str %1 "\t" (join "\t" %2)) v matrix)]
    (println row)))


(def named-fns
  [["Longest Common Subsequence" metric/lcs-sim]
   ["Cosine Distance" metric/cosine-sim]
   ["Jaccard Distance" metric/jaccard-sim]
   ["Jaro-Winkler Distance" metric/jaro-winkler-sim]
   ["Levenshtein Distance" metric/levenshtein-sim]
   ["Levenshtein Distance with free delete" (metric/levenshtein-sim-fn {:delete 0})]
   ["Levenshtein Distance with free substitute" (metric/levenshtein-sim-fn {:substitute 0})]
   ["Levenshtein Distance with free insert" (metric/levenshtein-sim-fn {:insert 0})]])


(defn -main
  [& _]
  (let [initiae (-> (load-fixture)
                    (flatten-fixture))]
    (doseq [[s f] named-fns]
      (println)
      (println s)
      (println "---")
      (print-matrix initiae (matrix/gen-symmetric f initiae))
      (println "---"))))
