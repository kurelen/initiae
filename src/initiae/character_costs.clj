(ns initiae.character-costs
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [lower-case]]))


(defn load-substitution-costs
  "Loads character substitution costs from EDN configuration."
  []
  (with-open [r (io/reader (io/resource "weights/substitute.edn"))]
    (edn/read (java.io.PushbackReader. r))))


(defn create-substitution-cost-fn
  "Creates a character substitution cost function from a cost lookup table."
  [cost-lookup]
  (fn [a b]
    (let [c (lower-case a)
          d (lower-case b)]
      (double (or (and (= c d) 0.0)
                  (cost-lookup [c d])
                  (cost-lookup [d c])
                  1.0)))))


(def paleographic-costs
  "Pre-configured character substitution costs for medieval manuscripts."
  {:substitute
   (create-substitution-cost-fn
     (:substitute (load-substitution-costs)))})
