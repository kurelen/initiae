(ns initiae.matrix)


(defn gen-symmetric
  "Generates an `n x n` symmetric matrix from a vector `v` and a symmetric binary function `f`.
  Each element A_ij of the resulting matrix is computed as (f v_i v_j), ensuring symmetry:
  A_ij = A_ji = (f (v i) (v j)).

  Arguments:
  - f: A symmetric binary function taking two elements of `v` and returning a scalar.
  - v: A vector of elements.

  Returns:
  - A nested vector (matrix) of size `n x n`, symmetric across the diagonal."
  [f v]
  (let [n (count v)
        ms
        (into {}
              (for [i (range n)
                    j (range i n)]
                [[i j] (f (nth v i) (nth v j))]))]
    (vec
      (for [i (range n)]
        (vec
          (for [j (range n)]
            (get ms (if (<= i j) [i j] [j i]))))))))
