(ns initiae.distance
  (:import
    [org.apache.commons.text.similarity
      LevenshteinDistance
      LevenshteinDetailedDistance]))

(defn levenshtein
  "Returns distance of two string"
  [s1 s2]
  (.apply (LevenshteinDistance.) s1 s2))

(defn weighted-levenshtein
  "Returns distance of two string"
  [opts]
  (let [{:keys [distance insert substitute delete]
         :or {distance 1 insert 1 substitute 1 delete 1}}
        opts]
    (fn [] [distance insert substitute delete])))


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
