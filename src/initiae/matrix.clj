(ns initiae.matrix)


(defn gen-symmetric
  "Creates a symmetric matrix by evaluating a bifunction f onto each ordered pair of elements of a vector v"
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
