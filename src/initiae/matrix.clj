(ns initiae.matrix)


(defn symmetric
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
  - A nested vector (matrix) of shape `n x m`, where `n = count v`, and `m = count w` (or `n` if `w` is omitted)."
  ([f v]
   (mapv
     (fn [a] (mapv (partial f a)
                   v))
       v))
  ([f v w]
   (mapv
     (fn [a] (mapv (partial f a)
                   w))
       v)))
