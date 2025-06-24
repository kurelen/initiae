(ns initiae.similarize)


(defn similarize
  [dist-fn]
  (fn [s1 s2] (- 1 (/ (dist-fn s1 s2) (max (count s1) (count s2))))))
