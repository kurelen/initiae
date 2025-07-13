(ns initiae.matrix
  (:require
    [clojure.core.matrix :as m]))


;; Set vectorz as the default implementation for better performance
(m/set-current-implementation :vectorz)


(defn symmetric
  "Generates an `n x n` symmetric matrix from a vector `v` and a symmetric binary function `f`.
  Each element A_ij of the resulting matrix is computed as (f v_i v_j), ensuring symmetry:
  A_ij = A_ji = (f (v i) (v j)).

  This implementation only computes the upper triangle and mirrors values for efficiency.

  Arguments:
  - f: A symmetric binary function taking two elements of `v` and returning a scalar.
  - v: A vector of elements.

  Returns:
  - A core.matrix matrix of size `n x n`, symmetric across the diagonal."
  [f v]
  (let [n (count v)]
    (m/compute-matrix [n n]
                      (fn [i j]
                        (if (<= i j)
                          (f (nth v i) (nth v j))
                          (f (nth v j) (nth v i)))))))


(defn pairwise
  "Constructs a matrix by applying a binary function `f` to all ordered pairs of elements
  from one or two input vectors.

  Arity 2:
  - Given a vector `v`, returns an `n x n` matrix where the element at position `[i j]`
    is `(f (v i) (v j))`.

  Arity 3:
  - Given vectors `v` and `w`, returns an `n x m` matrix where the element at `[i j]`
    is `(f (v i) (w j))`.

  Arguments:
  - f: A binary function of two arguments.
  - v: A vector of elements.
  - w (optional): A second vector of elements.

  Returns:
  - A core.matrix matrix of shape `n x m`, where `n = count v`, and `m = count w` (or `n` if `w` is omitted)."
  ([f v]
   (pairwise f v v))
  ([f v w]
   (m/compute-matrix [(count v) (count w)]
                     (fn [i j] (f (nth v i) (nth w j))))))


;; Utility functions for working with similarity/distance matrices

(defn matrix->nested-vec
  "Convert a core.matrix matrix back to nested vectors for compatibility."
  [matrix]
  (m/to-nested-vectors matrix))


(defn print-matrix
  "Pretty print a matrix with optional labels and formatting."
  ([matrix] (print-matrix matrix {}))
  ([matrix {:keys [labels precision] :or {precision 3}}]
   (let [rows (m/row-count matrix)
         cols (m/column-count matrix)
         fmt-str (str "%." precision "f")]
     (when labels
       (print "\t")
       (doseq [label labels] (print (format "%8s" label)))
       (println))
     (dotimes [i rows]
       (when labels (print (format "%8s" (nth labels i))))
       (dotimes [j cols]
         (print (format (str "%8" fmt-str) (m/mget matrix i j))))
       (println)))))


(defn matrix-stats
  "Generate basic statistics about a matrix."
  [matrix]
  {:shape [(m/row-count matrix) (m/column-count matrix)]
   :min (m/emin matrix)
   :max (m/emax matrix)
   :mean (m/scalar (m/div (m/esum matrix) (m/ecount matrix)))
   :symmetric? (m/equals matrix (m/transpose matrix))})


(defn threshold-matrix
  "Set all values below threshold to 0, useful for sparsifying similarity matrices."
  [matrix threshold]
  (m/emap #(if (< % threshold) 0.0 %) matrix))


(defn normalize-matrix
  "Normalize matrix values to [0,1] range."
  [matrix]
  (let [min-val (m/emin matrix)
        max-val (m/emax matrix)
        range (- max-val min-val)]
    (if (zero? range)
      matrix
      (m/div (m/sub matrix min-val) range))))
