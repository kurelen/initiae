(ns initiae.text-metric
  (:import
    (info.debatty.java.stringsimilarity
      Cosine
      Jaccard
      JaroWinkler
      Levenshtein
      LongestCommonSubsequence
      NGram)))


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
  (/ (lcs-dist s1 s2) (+ (count s1) (count s2))))


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
  "Returns levenshtein distance between two string. Can be limted (int)"
  ([s1 s2] (/ (levenshtein-dist s1 s2) (max (count s1) (count s2))))
  ([limit s1 s2] (/ (levenshtein-dist limit s1 s2)
                    (max (count s1) (count s2) limit))))
