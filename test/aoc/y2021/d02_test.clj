(ns aoc.y2021.d02-test
  (:require [aoc.y2021.d02 :as sut]
            [clojure.test :refer [deftest is testing]]))

(def sample
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(deftest part-1-example
  (testing "calculates horizontal * depth product"
    (is (= 150 (sut/part-1 sample)))))

(deftest part-2-example
  (testing "applies aim while calculating product"
    (is (= 900 (sut/part-2 sample)))))
