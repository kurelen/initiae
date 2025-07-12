(ns initiae.weight-function
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [lower-case]]))


(defn load-weight-definition
  []
  (with-open
    [r (io/reader
         (io/resource "weights/substitute.edn"))]
    (edn/read (java.io.PushbackReader. r))))


(defn substitute-definition
  [lookup]
  (fn [a b]
    (or (lookup [a b])
        (lookup [b a])
        (if (= (lower-case a) (lower-case b))
          0.0
          1.0))))


(def generated-weight-fn
  {:substitute
   (substitute-definition (load-weight-definition))})
