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


(def weights
  {:substitue (fn [a b] (if (= a b) 0.0 0.5))
   :delete (fn [c] (if (#{\a \e \i \o \u} c) 0.2 1.0))
   :insert (constantly 0.8)})


(def named-fns
  [["Longest Common Subsequence" metric/lcs-sim]
   ["Cosine Distance" metric/cosine-sim]
   ["Jaccard Distance" metric/jaccard-sim]
   ["Jaro-Winkler Distance" metric/jaro-winkler-sim]
   ["Levenshtein Distance" metric/levenshtein-sim]
   ["Weighted Levenshtein distance" (metric/weighted-levenshtein-dist-fn weights)]
   ["Weighted Levenshtein similarity" (metric/weighted-levenshtein-sim-fn weights)]])


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
