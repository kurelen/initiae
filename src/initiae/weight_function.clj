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
    (let [c (lower-case a)
          d (lower-case b)]
      (double (or (and (= c d) 0.0)
                  (lookup [c d])
                  (lookup [d c])
                  1.0)))))


(def generated-weight-fn
  {:substitute
   (substitute-definition
     (:substitute (load-weight-definition)))})
