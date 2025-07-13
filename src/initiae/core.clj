(ns initiae.core
  (:gen-class)
  (:require
    [clojure.core.matrix :as m]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]
    [clojure.string :refer [join]]
    [initiae.clustering :as c]
    [initiae.matrix :as matrix]
    [initiae.text-metric :as metric]
    [initiae.weight-function :refer [generated-weight-fn]]))


(defn load-fixture-edn
  []
  (with-open
    [r (io/reader
         (io/resource "fixtures/initia_variants.edn"))]
    (edn/read (java.io.PushbackReader. r))))


(defn flatten-fixture
  [data]
  (->> data
       (mapcat #(reduce into [] (vals %)))))


(defn load-fixture-list
  []
  (with-open
    [r (io/reader
         (io/resource "fixtures/initiae.txt"))]
    (vec (line-seq r))))


(defn print-matrix
  [matrix v]
  (doseq [row (map #(str %1 "\t" (join "\t" %2)) v matrix)]
    (println row)))


(def named-fns
  [["NGram similarity" metric/ngram-sim]
   ["Longest Common Subsequence distance" metric/lcs-dist]
   ["Longest Common Subsequence similarity" metric/lcs-sim]
   ["Cosine distance" metric/cosine-dist]
   ["Cosine similarity" metric/cosine-sim]
   ["Jaccard distance" metric/jaccard-dist]
   ["Jaccard similarity" metric/jaccard-sim]
   ["Jaro-Winkler distance" metric/jaro-winkler-dist]
   ["Jaro-Winkler similarity" metric/jaro-winkler-sim]
   ["Levenshtein distance" metric/levenshtein-dist]
   ["Levenshtein similarity" metric/levenshtein-sim]
   ["Damerau distance" metric/damerau-dist]
   ["Damerau similarity" metric/damerau-sim]
   ["Weighted Levenshtein distance" (metric/weighted-levenshtein-dist-fn generated-weight-fn)]
   ["Weighted Levenshtein similarity" (metric/weighted-levenshtein-sim-fn generated-weight-fn)]])


(defn -main
  [& _]
  (let [v (load-fixture-list)]
    (-> (metric/weighted-levenshtein-sim-fn generated-weight-fn)
        (matrix/symmetric v)
        ; (print-matrix v)
        (m/matrix)
        (c/run-mcl 4.0 100 1e-5)
        (c/extract-clusters)
        pprint)))


(comment (defn -main
  [& _]
  ;;  (let [initiae (-> (load-fixture-edn)
  ;;                    (flatten-fixture))]
  (let [initiae (load-fixture-list)]
    (doseq [[s f] named-fns]
      (println s)
      (print-matrix initiae (matrix/symmetric f initiae))
      (println)))))
