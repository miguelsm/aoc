#!/usr/bin/env bb

;; Helpers

(def path (subs *file* 0 (inc (str/last-index-of *file* "/"))))

(defn file->int-seq [f]
  (->> f (str path) slurp str/split-lines (map #(Integer/parseInt %))))

(defn file->str-seq [f]
  (->> f (str path) slurp str/split-lines))

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

;;
;; Day 2
;;

(defn day-2-1 [input]
  (let [[depth pos]
        (reduce (fn [[depth pos] [dir x]]
                  (case dir
                    "down"    [(+ depth x) pos]
                    "forward" [depth (+ pos x)]
                    "up"      [(- depth x) pos]))
                [0 0]
                input)]
    (* depth pos)))

(defn day-2-2 [input]
  (let [[_ depth pos]
        (reduce (fn [[aim depth pos] [dir x]]
                  (case dir
                    "down"    [(+ aim x) depth pos]
                    "forward" [aim (+ depth (* aim x)) (+ pos x)]
                    "up"      [(- aim x) depth pos]))
                [0 0 0]
                input)]
    (* depth pos)))

(let [input (->> (file->str-seq "input/02")
                 (map (fn [command]
                        (let [[dir x] (str/split command #" ")]
                          [dir (Integer/parseInt x)]))))]
  (println "Day 2 -> part 1:" (day-2-1 input))
  (println "      -> part 2:" (day-2-2 input)))
