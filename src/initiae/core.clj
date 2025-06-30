(ns initiae.core
  (:gen-class)
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [join]]
    [initiae.matrix :as matrix]
    [initiae.text-metric :as metric]))


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
  [v matrix]
  (doseq [row (map #(str %1 "\t" (join "\t" %2)) v matrix)]
    (println row)))


(def weights
  {:substitue (fn [a b] (if (= a b) 0.0 0.5))
   :delete (fn [c] (if (#{\a \e \i \o \u} c) 0.2 1.0))
   :insert (constantly 0.8)})


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
   ["Damerau similarity" metric/damerau-dist]
   ["Weighted Levenshtein distance" (metric/weighted-levenshtein-dist-fn weights)]
   ["Weighted Levenshtein similarity" (metric/weighted-levenshtein-sim-fn weights)]])


(defn -main
  [& _]
  ;;  (let [initiae (-> (load-fixture-edn)
  ;;                    (flatten-fixture))]
  (let [initiae (load-fixture-list)]
    (doseq [[s f] named-fns]
      (println s)
      (print-matrix initiae (matrix/gen-symmetric f initiae))
      (println))))
