(ns initiae.weight-function
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]))


(defn load-weight-definition
  []
  (with-open
    [r (io/reader
         (io/resource "weights/substitute.edn"))]
    (edn/read (java.io.PushbackReader. r))))
