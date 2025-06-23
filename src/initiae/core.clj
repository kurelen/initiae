(ns initiae.core
  (:gen-class)
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]))


(defn load-fixture
  []
  (with-open
    [r (io/reader
         (io/resource "fixtures/initia_variants.edn"))]
    (edn/read (java.io.PushbackReader. r))))


(defn -main
  [& args]
  (println "Welcome to Initiae!")
  (println "Command-line args:" args))
