(ns initiae.core
  (:gen-class)
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [join]]
    [initiae.distance :as dist]))


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


(def dist-fns
  [["Longest Common Subsequence" dist/lcs]
   ["Cosine Distance" dist/cosine]
   ["Jaccard Distance" dist/jaccard]
   ["Jaro-Winkler Distance" dist/jaro-winkler]
   ["Levenshtein Distance" dist/levenshtein]
   ["Levenshtein Distance with free delete" (dist/levenshtein-fn {:delete 0})]
   ["Levenshtein Distance with free substitute" (dist/levenshtein-fn {:substitute 0})]
   ["Levenshtein Distance with free insert" (dist/levenshtein-fn {:insert 0})]])


(defn -main
  [& _]
  (let [initiae (-> (load-fixture)
                    (flatten-fixture)
                    (shuffle))]
  (doseq [[fn-name dist-fn] dist-fns]
      (println)
      (println fn-name)
      (println "---")
      (print-matrix initiae (dist/distance-matrix initiae dist-fn))
      (println "---"))))
