(ns aoc.y2021.d01-test
  (:require [aoc.y2021.d01 :as sut]
            [clojure.test :refer [deftest is testing]]))

(def sample
  "199
200
208
210
200
207
240
269
260
263")

(deftest part-1-example
  (testing "counts simple depth increases"
    (is (= 7 (sut/part-1 sample)))))

(deftest part-2-example
  (testing "counts sliding window increases"
    (is (= 5 (sut/part-2 sample)))))
