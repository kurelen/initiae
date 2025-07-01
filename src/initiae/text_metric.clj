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


(defn ngram-dist
  "Returns the N-gram distance between strings `s1` and `s2`. 
  Optionally accepts an integer `:ngram-length` via an options map specifying the N-Gram size."
  ([s1 s2]
   (.distance (NGram.) s1 s2))
  ([s1 s2 {:keys [ngram-length] :or {ngram-length 2}}]
   (.distance (NGram. ngram-length) s1 s2)))


(defn lcs-dist
  "Returns the Longest Common Subsequence (LCS) distance between strings `s1` and `s2`."
  [s1 s2]
  (.distance (LongestCommonSubsequence.) s1 s2))


(defn lcs-sim
  "Returns the Longest Common Subsequence (LCS) similarity between strings `s1` and `s2`."
  [s1 s2]
  (.distance (MetricLCS.) s1 s2))


(defn cosine-dist
  "Returns the cosine distance between strings `s1` and `s2`, based on token frequency."
  [s1 s2]
  (.distance (Cosine.) s1 s2))


(defn cosine-sim
  "Returns the cosine similarity between strings `s1` and `s2`, based on token frequency."
  [s1 s2]
  (.similarity (Cosine.) s1 s2))


(defn jaccard-dist
  "Returns the Jaccard distance between strings `s1` and `s2`. 
  Optionally accepts an integer `:shingle-size` via an options map specifying
    the size of shingles the strings are split into."
  ([s1 s2] (.distance (Jaccard.) s1 s2))
  ([s1 s2 {:keys [shingle-size] :or {shingle-size 3}}]
   (.distance (Jaccard. shingle-size) s1 s2)))


(defn jaccard-sim
  "Returns the Jaccard similarity between strings `s1` and `s2`. 
  Optionally accepts an integer `:shingle-size` via an options map specifying
    the size of shingles the strings are split into."
  ([s1 s2] (.similarity (Jaccard.) s1 s2))
  ([s1 s2 {:keys [shingle-size] :or {shingle-size 3}}]
   (.similarity (Jaccard. shingle-size) s1 s2)))


(defn jaro-winkler-dist
  "Returns the Jaro-Winkler distance between strings `s1` and `s2`. 
  Optionally accepts a double `:threshold` via an options map to control prefix scaling."
  ([s1 s2] (.distance (JaroWinkler.) s1 s2))
  ([s1 s2 {:keys [threshold] :or {threshold 0.7}}]
   (.distance (JaroWinkler. threshold) s1 s2)))


(defn jaro-winkler-sim
  "Returns the Jaro-Winkler similarity between strings `s1` and `s2`. 
  Optionally accepts a double `:threshold` via an options map to control prefix scaling."
  ([s1 s2] (.similarity (JaroWinkler.) s1 s2))
  ([s1 s2 {:keys [threshold] :or {threshold 0.7}}]
   (.similarity (JaroWinkler. threshold) s1 s2)))


(defn levenshtein-dist
  "Returns the Levenshtein distance between strings `s1` and `s2`. 
  Optionally accepts an integer `:limit` via an options map to bound computation."
  ([s1 s2] (.distance (Levenshtein.) s1 s2))
  ([s1 s2 {:keys [limit] :or {limit Integer/MAX_VALUE}}]
   (.distance (Levenshtein.) s1 s2 limit)))


(defn levenshtein-sim
  "Returns the Levenshtein similarity between strings `s1` and `s2`. 
  Optionally accepts an integer `:limit` via an options map to bound computation."
  ([s1 s2] (- 1.0
              (/ (levenshtein-dist s1 s2)
                 (max (count s1) (count s2)))))
  ([s1 s2 {:keys [limit] :or {limit Integer/MAX_VALUE}}]
   (- 1.0
      (/ (levenshtein-dist s1 s2 {:limit limit})
         (min limit (max (count s1) (count s2)))))))


(defn damerau-dist
  "Returns the Damerau-Levenshtein distance between strings `s1` and `s2`."
  ([s1 s2] (.distance (Damerau.) s1 s2)))


(defn damerau-sim
  "Returns the Damerau-Levenshtein similarity between strings `s1` and `s2`."
  [s1 s2]
  (- 1.0
     (/ (damerau-dist s1 s2)
        (max (count s1) (count s2)))))


(def ^:private default-insert-delete-cost (constantly 1.0))


(defn- default-substitute-cost
  [c1 c2]
  (if (= c1 c2) 0.0 1.0))


(defn weighted-levenshtein-dist-fn
  "Returns a function that computes weighted Levenshtein distance between two strings. 

  Options:
    - `:substitute` — binary cost function for substitution (default: identity match/mismatch)
    - `:insert`     — unary cost function for insertions (default: 1.0)
    - `:delete`     — unary cost function for deletions (default: 1.0)
    - `:limit`      — integer upper bound for distance (default: Double/MAX_VALUE)

  Caution: Cost functions should return value between 0.0 and 1.0"
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
  "Returns a function that computes weighted Levenshtein similarity between two strings.

  Options:
    - Same as `weighted-levenshtein-dist-fn`
    - `:upperbound` — function to compute max possible distance (default: max of length of strings)"
  [opts]
  (let [{:keys [upperbound]
         :or {upperbound default-upperbound}} opts
        dist-fn (weighted-levenshtein-dist-fn opts)]
    (fn [s1 s2]
      (- 1.0 (/ (dist-fn s1 s2)
                (upperbound (count s1) (count s2)))))))
