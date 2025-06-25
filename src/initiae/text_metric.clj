(ns initiae.text-metric
  (:import
    (info.debatty.java.stringsimilarity
      CharacterInsDelInterface
      CharacterSubstitutionInterface
      Cosine
      Damerau
      Jaccard
      JaroWinkler
      Levenshtein
      LongestCommonSubsequence
      MetricLCS
      NGram
      WeightedLevenshtein)))


(defn ngram-sim
  "Return similarity of two strings s1 s2. Can configure ngram length n (int n)"
  ([s1 s2]
   (.distance (NGram.) s1 s2))
  ([n s1 s2]
   (.distance (NGram. n) s1 s2)))


(defn lcs-dist
  "Returns longest common subsequence distance between two strings"
  [s1 s2]
  (.distance (LongestCommonSubsequence.) s1 s2))


(defn lcs-sim
  "Returns longest common subsequence similarity between two strings"
  [s1 s2]
  (.distance (MetricLCS.) s1 s2))


(defn cosine-dist
  "Returns cosine distance between two strings"
  [s1 s2]
  (.distance (Cosine.) s1 s2))


(defn cosine-sim
  "Returns cosine similarity between two strings"
  [s1 s2]
  (.similarity (Cosine.) s1 s2))


(defn jaccard-dist
  "Returns jaccard distance between two strings. Allows to be configured using k-shingles (k int)"
  ([s1 s2] (.distance (Jaccard.) s1 s2))
  ([k s1 s2] (.distance (Jaccard. k) s1 s2)))


(defn jaccard-sim
  "Returns jaccard similarity between two strings. Allows to be configured using k-shingles (k int)"
  ([s1 s2] (.similarity (Jaccard.) s1 s2))
  ([k s1 s2] (.similarity (Jaccard. k) s1 s2)))


(defn jaro-winkler-dist
  "Returns jaccard distance between two strings. Can be configured using threshold (double)"
  ([s1 s2] (.distance (JaroWinkler.) s1 s2))
  ([threshold s1 s2] (.distance (JaroWinkler. threshold) s1 s2)))


(defn jaro-winkler-sim
  "Returns jaccard similarity between two strings. Can be configured using threshold (t double)"
  ([s1 s2] (.similarity (JaroWinkler.) s1 s2))
  ([threshold s1 s2] (.similarity (JaroWinkler. threshold) s1 s2)))


(defn levenshtein-dist
  "Returns levenshtein distance between two string. Can be limted (int)"
  ([s1 s2] (.distance (Levenshtein.) s1 s2))
  ([limit s1 s2] (.distance (Levenshtein.) s1 s2 limit)))


(defn levenshtein-sim
  "Returns levenshtein similarity between two string. Can be limted (int)"
  ([s1 s2] (- 1.0
              (/ (levenshtein-dist s1 s2)
                 (max (count s1) (count s2)))))
  ([limit s1 s2] (- 1.0
                    (/ (levenshtein-dist limit s1 s2)
                       (min limit (max (count s1) (count s2)))))))


(defn damerau-dist
  "Returns damarau-levenshtein distance between two string. Can be limted (int)"
  ([s1 s2] (.distance (Damerau.) s1 s2)))


(defn damerau-sim
  "Returns damarau-levenshtein similarity between two string"
  [s1 s2]
  (- 1.0
     (/ (damerau-dist s1 s2)
        (max (count s1) (count s2)))))


(def ^:private default-insert-delete-cost (constantly 1.0))


(defn- default-substitute-cost
  [c1 c2]
  (if (= c1 c2) 0.0 1.0))


(defn weighted-levenshtein-dist-fn
  "Returns weighted levenshtein distance function between two string. Options have to be supplied with keys :substitute  (binary cost fn) :insert (unary cost fn) :delete (unary cost fn) :limit (int)"
  [opts]
  (let [{:keys [substitute insert delete limit]
         :or {substitute default-substitute-cost
              insert default-insert-delete-cost
              delete default-insert-delete-cost
              limit Double/MAX_VALUE}} opts
        char-sub (reify CharacterSubstitutionInterface
                   (cost [_ c1 c2] (substitute c1 c2)))
        char-change (reify CharacterInsDelInterface
                      (insertionCost [_ c] (insert c))

                      (deletionCost [_ c] (delete c)))
        o (WeightedLevenshtein. char-sub char-change)]
    (fn [s1 s2] (.distance o s1 s2 limit))))


(defn- default-upperbound
  [s1 s2]
  (max s1 s2))


(defn weighted-levenshtein-sim-fn
  "Returns weighted levenshtein similarity function between two string. Options have to be supplied with keys :substitute  (binary cost fn) :insert (unary cost fn) :delete (unary cost fn) :limit (int) and :upperbound (cost fn in length of strings and limit)"
  [opts]
  (let [{:keys [upperbound]
         :or {upperbound default-upperbound}} opts
        dist-fn (weighted-levenshtein-dist-fn opts)]
    (fn [s1 s2]
      (- 1.0 (/ (dist-fn s1 s2)
                (upperbound (count s1) (count s2)))))))
