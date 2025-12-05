(ns aoc.y2021.d01
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [lines (if (string? input)
                (str/split-lines input)
                (map str input))]
    (map #(Integer/parseInt %) lines)))

(defn part-1 [input]
  (->> (parse input)
       (partition 2 1)
       (reduce (fn [acc [a b]] (if (< a b) (inc acc) acc)) 0)))

(defn part-2 [input]
  (->> (parse input)
       (partition 3 1)
       (map #(apply + %))
       part-1))

(comment
  (let [year  2021
        day   "01"
        input (parse (slurp (format "resources/inputs/%s/day%s.txt" year day)))]
    (part-1 input))
  )
