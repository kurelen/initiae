(ns clerk-dev
  (:require
    [nextjournal.clerk :as clerk]))


(defn start!
  []
  (clerk/serve! {:browse? true :port 7777})
  (clerk/show! "notebooks/clustering_analysis.clj"))


(defn build!
  []
  (clerk/build! {:paths ["notebooks/clustering_analysis.clj"]
                 :out-path "docs"}))


(comment
  (start!)
  (build!)
  )
