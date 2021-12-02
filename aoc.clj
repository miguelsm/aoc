#!/usr/bin/env bb

;; Helpers

(def path (subs *file* 0 (inc (str/last-index-of *file* "/"))))

(defn file->int-seq [f]
  (->> f (str path) slurp str/split-lines (map #(Integer/parseInt %))))

;;
;; Day 1
;;

(defn day-1-1 [input]
  (->> input
       (partition 2 1)
       (reduce (fn [acc [a b]] (if (< a b) (inc acc) acc)) 0)))

(defn day-1-2 [input]
  (->> input
       (partition 3 1)
       (map #(apply + %))
       day-1-1))

(let [input (file->int-seq "input/01")]
  (println "Day 1 -> part 1:" (day-1-1 input))
  (println "      -> part 2:" (day-1-2 input)))
