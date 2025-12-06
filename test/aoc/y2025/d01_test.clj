(ns aoc.y2025.d01-test
  (:require [aoc.y2025.d01 :as sut]
            [clojure.test :refer [deftest is testing]]))

(def sample
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(deftest part-1-example
  (testing "counts times dial ends at 0 after rotation"
    (is (= 3 (sut/part-1 sample)))))

(deftest part-2-example
  (testing "counts times dial crosses 0"
    (is (= 6 (sut/part-2 sample)))))
