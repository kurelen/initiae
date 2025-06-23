(ns initiae.distance
  (:import
    (org.apache.commons.text.similarity
      LevenshteinDetailedDistance
      LevenshteinDistance)))


(defn levenshtein
  "Returns distance of two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))


(defn weighted-levenshtein
  "Returns distance function. Supply"
  ([] levenshtein)
  ([opts]
   (let [{:keys [insert substitute delete]
          :or {insert 1 substitute 1 delete 1}}
         opts]
     (fn [s1 s2]
       (let [r (.apply (LevenshteinDetailedDistance.) s1 s2)]
         (+ (* insert (.getInsertCount r))
            (* substitute (.getSubstituteCount r))
            (* delete (.getDeleteCount r))))))))


(defn levenshtein
  "Returns distance of two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))


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
