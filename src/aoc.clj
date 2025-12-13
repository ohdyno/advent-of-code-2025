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

(defn rotate-dial
  "Given a dial at dial-location, rotate the dial for the number of directional-clicks, and return the location at the end of the rotation"
  [dial-location directional-clicks]
  (-> dial-location
      (+ directional-clicks)
      (mod 100)))

(defn day-1-part-1-recursion
  [dial-start turn-inputs]
  (loop [[parsed-direction & parsed-directions] (map parse-dial-direction
                                                  turn-inputs)
         zero-counts 0
         end-locations [dial-start]]
    (let [dial-location (last end-locations)]
      (if parsed-direction
        (let [end-location (rotate-dial dial-location parsed-direction)
              zero-count-increment-fn #(if (= 0 %) 1 0)]
          (recur parsed-directions (+ zero-counts (zero-count-increment-fn end-location))(conj end-locations end-location)))
        zero-counts))))

(defn day-1-part-1-sequences
  [turn-inputs]
  (->> turn-inputs
       (map parse-dial-direction)
       (calculate-end-location dial-start)
       (filter #(= 0 %))
       (count)
  ))

(defn day-1-part-2 [turn-inputs])

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)] (time (day-1-part-1-recursion dial-start input-lines))))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)] (time (day-1-part-1-sequences input-lines))))

