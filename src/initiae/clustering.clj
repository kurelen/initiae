(ns initiae.clustering
  (:require
    [clojure.core.matrix :as m]))


(m/set-current-implementation :vectorz)


(defn normalize-columns [matrix]
  (let [cols (m/columns matrix)
        col-sums (map #(m/esum %) cols)]
    (->> (map (fn [col sum]
                (if (zero? sum)
                  col
                  (m/div col sum)))
              cols col-sums)
         m/matrix
         m/transpose)))


(defn inflate
  [matrix inflation-factor]
  (->> (m/emap #(Math/pow % inflation-factor) matrix)
       (normalize-columns)))


(defn expand
  [matrix]
  (m/mmul matrix matrix)) ; einfache Expansion (power = 2)

(defn mcl-step
  [matrix inflation-factor]
  (-> matrix
      expand
      (inflate inflation-factor)))


(defn converged?
  [a b eps]
  (< (m/eseq (m/sub a b))
     eps))


(defn run-mcl
  [adj-matrix inflation-factor max-steps eps]
  (loop [current (inflate adj-matrix inflation-factor)
         step 0]
    (let [next (mcl-step current inflation-factor)]
      (if (or (>= step max-steps)
              (< (m/esum (m/abs (m/sub current next))) eps))
        next
        (recur next (inc step))))))


(defn extract-clusters
  [matrix]
  (->> (m/rows matrix)
       (map-indexed (fn [i row]
                      (when (some #(> % 0.01) row)
                        [i (keep-indexed #(when (> %2 0.01) %1) row)])))
       (remove nil?)
       (group-by second)
       (vals)
       (map #(map first %))))
