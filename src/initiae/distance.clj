(ns initiae.distance
  (:import [org.apache.commons.text.similarity LevenshteinDistance]))

(defn weighted-levenshtein
  "Returns distance of two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))
