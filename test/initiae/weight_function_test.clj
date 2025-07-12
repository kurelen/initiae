(ns initiae.weight-function-test
  (:require
    [clojure.test :refer [deftest is]]
    [initiae.weight-function :as w]))


(deftest test-load-weight-definition
  (let [weights (w/load-weight-definition)]
    (is (not (= (:substitute weights)
                nil)))))
