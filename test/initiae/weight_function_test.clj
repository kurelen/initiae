(ns initiae.weight-function-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.weight-function :as w]))


(deftest test-load-weight-definition
  (let [weights (w/load-weight-definition)]
    (is (not (= (:substitute weights)
                nil)))))


(deftest test-substitute-definition
  (let [sfn (w/substitute-definition
              {["a" "b"] 0.2
               ["b" "c"] 0.3})]
    (is (= (sfn "a" "B") 0.2))
    (is (= (sfn "a" "b") 0.2))
    (is (= (sfn "b" "a") 0.2))
    (is (= (sfn "B" "A") 0.2))
    (is (= (sfn "c" "B") 0.3))
    (is (= (sfn "c" "C") 0.0))
    (is (= (sfn "c" "c") 0.0))
    (is (= (sfn "c" "d") 1.0))
    (is (= (sfn "d" "c") 1.0))))


(deftest test-generated-weight-fn
  (let [sfn (:substitute w/generated-weight-fn)]
    (is (= (sfn "a" "O") 0.3))
    (is (= (sfn "A" "Ä") 0.7))
    (is (= (sfn "ä" "e") 0.0))
    (is (= (sfn "e" "E") 0.0))))
