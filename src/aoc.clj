(ns aoc
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]))
(comment
  "day 1, part 1"
  (repl/doc re-groups)
  (mod (+ dial-start -5) 99)
  (mod (+ dial-start (- 99 5)) 99)
  (+ dial-start 1))

(def dial-start 50)

(defn parse-dial-direction
  "Parse a single dial direction e.g. L68 into directional clicks e.g. -68"
  [input]
  (let [[_ _ direction clicks]
          (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)
        amount (Integer/parseInt clicks)]
    (if (= "L" direction) (- amount) amount)))

(defn calculate-end-location
  [starting-at dial-directions]
  (map-indexed (fn [index _]
                 (as-> index v
                   (inc v)
                   (take v dial-directions)
                   (reduce + starting-at v)
                   (mod v 100)))
               dial-directions))

(defn day-1-part-1
  [turn-inputs]
  (->> turn-inputs
       (map parse-dial-direction)
       (calculate-end-location dial-start)
       (filter #(= 0 %))
       (count)))

(defn day-1-part-2 [turn-inputs])

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (day-1-part-1 (line-seq rdr)))