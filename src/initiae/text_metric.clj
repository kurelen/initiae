(ns initiae.text-metric
  (:import
    [org.apache.commons.text.similarity
      CosineDistance
      JaccardDistance
      JaccardSimilarity
      JaroWinklerDistance
      JaroWinklerSimilarity
      LevenshteinDetailedDistance
      LevenshteinDistance
      LongestCommonSubsequenceDistance]
    [info.debatty.java.stringsimilarity NGram]))

(defn ngram
  [s1 s2]
  (.distance (NGram.) s1 s2))

(defn- max-length-similarize
  [dist-fn]
  (fn [s1 s2]
    (- 1
       (float (/
                (dist-fn s1 s2)
                (max (count s1) (count s2)))))))


(defn lcs-dist
  "Returns longest common subsequence distance between two strings"
  [s1 s2]
  (.apply (LongestCommonSubsequenceDistance.) s1 s2))


(defn lcs-sim
  "Returns longest common subsequence similarity between two strings"
  [s1 s2]
  ((max-length-similarize lcs-dist) s1 s2))


(defn cosine-dist
  "Returns cosine distance between two strings"
  [s1 s2]
  (.apply (CosineDistance.) s1 s2))


(defn cosine-sim
  "Returns cosine similarity between two strings"
  [s1 s2]
  (- 1 (abs (cosine-dist s1 s2))))


(defn jaccard-dist
  "Returns jaccard distance between two strings"
  [s1 s2]
  (.apply (JaccardDistance.) s1 s2))


(defn jaccard-sim
  "Returns jaccard similarity between two strings"
  [s1 s2]
  (.apply (JaccardSimilarity.) s1 s2))


(defn jaro-winkler-dist
  "Returns jaccard distance between two strings"
  [s1 s2]
  (.apply (JaroWinklerDistance.) s1 s2))


(defn jaro-winkler-sim
  "Returns jaccard similarity between two strings"
  [s1 s2]
  (.apply (JaroWinklerSimilarity.) s1 s2))


(defn levenshtein-dist
  "Returns levenshtein distance between two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))


(defn levenshtein-sim
  "Returns levenshtein similarity between two string"
  [s1 s2]
  ((max-length-similarize levenshtein-dist) s1 s2))


(defn levenshtein-dist-fn
  "Returns levenshtein distance function with optional weights {:insert, :substitute, :delete}"
  ([] levenshtein-dist)
  ([weights]
   (let [{:keys [insert substitute delete]
          :or {insert 1 substitute 1 delete 1}}
         weights]
     (fn [s1 s2]
       (let [r (.apply (LevenshteinDetailedDistance.) s1 s2)]
         (+ (* insert (.getInsertCount r))
            (* substitute (.getSubstituteCount r))
            (* delete (.getDeleteCount r))))))))


(defn levenshtein-sim-fn
  "Returns levenshtein similarity function with optional weights {:insert, :substitute, :delete}"
  ([] levenshtein-sim)
  ([weights]
   (max-length-similarize (levenshtein-dist-fn weights))))


;; This is not correct
