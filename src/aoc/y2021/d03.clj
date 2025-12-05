(ns aoc.y2021.d03
  (:require [clojure.string :as str]))

(defn transpose [m]
  (apply (fn [& xs] (apply map list xs)) m))

(defn parse [input]
  (->> input str/split-lines (map seq) transpose))

(defn part-1 [input]
  (let [[g e] (reduce
                (fn [[g e] row]
                  (let [{times-0 \0 times-1 \1} (frequencies row)]
                    (if (< times-0 times-1)
                      [(str g 1) (str e 0)]
                      [(str g 0) (str e 1)])))
                ["" ""]
                (parse input))]
    (* (Integer/parseInt g 2)
       (Integer/parseInt e 2))))

(defn part-2 [_input]
  nil)
