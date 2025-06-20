(ns initiae.core
  (:gen-class))


(defn -main
  [& args]
  (println "Welcome to Initiae!")
  (println "Command-line args:" args))


(defn greet
  [name]
  (str "Hello, " name "!"))
