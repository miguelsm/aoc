(ns aoc.y2021.d02
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (map (fn [command]
              (let [[dir x] (str/split command #" ")]
                [dir (Integer/parseInt x)])))))

(defn part-1 [input]
  (let [[depth pos]
        (reduce (fn [[depth pos] [dir x]]
                  (case dir
                    "down"    [(+ depth x) pos]
                    "forward" [depth (+ pos x)]
                    "up"      [(- depth x) pos]))
                [0 0]
                (parse input))]
    (* depth pos)))

(defn part-2 [input]
  (let [[_ depth pos]
        (reduce (fn [[aim depth pos] [dir x]]
                  (case dir
                    "down"    [(+ aim x) depth pos]
                    "forward" [aim (+ depth (* aim x)) (+ pos x)]
                    "up"      [(- aim x) depth pos]))
                [0 0 0]
                (parse input))]
    (* depth pos)))
