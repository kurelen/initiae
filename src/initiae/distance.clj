(ns initiae.distance
  (:import
    (org.apache.commons.text.similarity
      CosineDistance
      JaccardDistance
      JaroWinklerDistance
      LevenshteinDetailedDistance
      LevenshteinDistance
      LongestCommonSubsequenceDistance)))


(defn lcs
  "Returns longest common subsequence distance between two strings"
  [s1 s2]
  (.apply (LongestCommonSubsequenceDistance.) s1 s2))


(defn cosine
  "Returns cosine distance between two strings"
  [s1 s2]
  (.apply (CosineDistance.) s1 s2))


(defn jaccard
  "Returns jaccard distance between two strings"
  [s1 s2]
  (.apply (JaccardDistance.) s1 s2))


(defn jaro-winkler
  "Returns jaccard distance between two strings"
  [s1 s2]
  (.apply (JaroWinklerDistance.) s1 s2))


(defn levenshtein
  "Returns distance of two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))


(defn levenshtein-fn
  "Returns levenshtein function with optional weights {:insert, :substitute, :delete}"
  ([] levenshtein)
  ([weights]
   (let [{:keys [insert substitute delete]
          :or {insert 1 substitute 1 delete 1}}
         weights]
     (fn [s1 s2]
       (let [r (.apply (LevenshteinDetailedDistance.) s1 s2)]
         (+ (* insert (.getInsertCount r))
            (* substitute (.getSubstituteCount r))
            (* delete (.getDeleteCount r))))))))


(defn distance-matrix
  [v distance-fn]
  (let [n (count v)
        distances
        (into {}
              (for [i (range n)
                    j (range i n)]
                [[i j] (distance-fn (nth v i) (nth v j))]))]
    (vec
      (for [i (range n)]
        (vec
          (for [j (range n)]
            (get distances (if (<= i j) [i j] [j i]))))))))
