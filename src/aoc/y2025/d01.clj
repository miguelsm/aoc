(ns aoc.y2025.d01
  (:require [clojure.string :as str]))

(def dial-start 50)  ; The number the dial starts at
(def dial-count 100) ; How many numbers around the dial

(defn parse [input]
  (str/split-lines input))

(defn parse-rotation
  "Returns an array with two elements:
    - A boolean indicating whether it's a left (`true`) or a right (`false`) rotation
    - The amount of position the rotation moves the dial in that direction"
  [rotation]
  (let [left?  (str/starts-with? rotation "L")
        amount (Integer/parseInt (subs rotation 1))]
    [left? amount]))

(defn rotation-result
  "Returns the number a dial pointing at `n` would point at after being
  rotated by `amount` in the direction indicated by `left?`."
  [[left? amount] n]
  (-> n
      (+ (* (if left? -1 1) amount))
      (mod dial-count)))

(defn count-zero-crossings
  "Returns the number of times the dial passes through '0' when applying
  the given rotation going from `dial-curr` to `dial-next`."
  [[left? amount] dial-curr dial-next]
  (+ (if (zero? dial-next) 1 0)
     (quot amount dial-count) ; a single rotation like R1000 would cause the dial to point at 0 ten times
     (if (zero? dial-curr)
       0
       (let [r (mod amount dial-count)]
         (if left?
           (if (< dial-curr r) 1 0)
           (if (< dial-count (+ dial-curr r)) 1 0))))))

(defn part-1 [input]
  (loop [rotations (parse input)
         dial-curr dial-start
         result    0]
    (if (empty? rotations)
      result
      (let [r         (parse-rotation (first rotations))
            dial-next (rotation-result r dial-curr)
            result    (if (zero? dial-next) (inc result) result)]
        (recur (rest rotations)
               dial-next
               result)))))

(defn part-2 [input]
  (loop [rotations (parse input)
         dial-curr dial-start
         result    0]
    (if (empty? rotations)
      result
      (let [r              (parse-rotation (first rotations))
            dial-next      (rotation-result r dial-curr)
            zero-crossings (count-zero-crossings r dial-curr dial-next)]
        #_(println (first rotations) dial-next x)
        (recur (rest rotations)
               dial-next
               (+ result zero-crossings))))))

(comment
  (parse-rotation "L68")
  (rotation-result '(true 5) 50)
  (count-zero-crossings '(true 68) 50 82)

  (let [input
        (slurp "resources/inputs/2025/day01.txt")
        #_(str/join "\n" ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"])]
    (println "---")
    [(part-1 input) (part-2 input)])
  )
