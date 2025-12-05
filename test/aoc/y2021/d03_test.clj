(ns aoc.y2021.d03-test
  (:require [aoc.y2021.d03 :as sut]
            [clojure.test :refer [deftest is testing]]))

(def sample
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(deftest part-1-example
  (testing "computes gamma * epsilon"
    (is (= 198 (sut/part-1 sample)))))
