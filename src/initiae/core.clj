(ns initiae.core
  (:gen-class)
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [initiae.distance :refer [distance-matrix weighted-levenshtein]]))


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
  [matrix]
  (doseq [row matrix]
    (println row)))


(defn -main
  [& _]
  (let [initiae (flatten-fixture (load-fixture))
        distances (distance-matrix initiae weighted-levenshtein)]
    (print-matrix initiae)
    (print-matrix distances)))
