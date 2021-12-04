#!/usr/bin/env bb

;; Helpers

(def path (subs *file* 0 (inc (str/last-index-of *file* "/"))))

(defn file->char-matrix [f]
  (->> f (str path) slurp str/split-lines (map seq)))

(defn file->int-seq [f]
  (->> f (str path) slurp str/split-lines (map #(Integer/parseInt %))))

(defn file->str-seq [f]
  (->> f (str path) slurp str/split-lines))

(defn transpose [m]
  (apply (fn [& xs] (apply map list xs)) m))

(defn print-day [d p1 p2]
  (println "Day" d "-> part 1:" p1)
  (println "      -> part 2:" p2))

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
  (print-day 1 (day-1-1 input) (day-1-2 input)))

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
  (print-day 2 (day-2-1 input) (day-2-2 input)))

;;
;; Day 3
;;

(defn day-3-1 [input]
  (let [[g e] (reduce
                (fn [[g e] row]
                  (let [{times-0 \0 times-1 \1} (frequencies row)]
                    (if (< times-0 times-1)
                      [(str g 1) (str e 0)]
                      [(str g 0) (str e 1)])))
                ["" ""]
                input)]
    (* (Integer/parseInt g 2)
       (Integer/parseInt e 2))))

(defn day-3-2 [input]
  )

(let [input (transpose (file->char-matrix "input/03"))]
  (print-day 3 (day-3-1 input) (day-3-2 input)))
