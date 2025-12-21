(ns day1.aco
  (:require [clojure.java.io :as io]))
(comment
  (remove-ns 'aoc-day-1))

(defprotocol DialCommandResults
  "A list of queries about the result of executing the dial command."
  (would-cross-zero? [_]
   "Would the command result in the dial crossing zero?")
  (end-location [_]
   "The end location that the dial would point at after executing the command.")
  (is-going-left? [_]
   "Is the command turning the dial to the left?"))

(defrecord DialCommand [start direction clicks]
  DialCommandResults
    (would-cross-zero? [_]
      (if (is-going-left? _) (>= clicks start) (>= clicks (- 100 start))))
    (end-location [_]
      (-> (if (is-going-left? _) (- start clicks) (+ start clicks))
          (mod 100)))
    (is-going-left? [_] (= direction :L)))

(defn- parse-directional-clicks
  "Parse a single dial direction e.g. L68 into directional clicks e.g. {:direction :l, :clicks 68}"
  [input]
  (let [[_ _ direction clicks]
        (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)
        amount (Integer/parseInt clicks)]
    {:direction (keyword direction), :clicks amount}))

(defn- calculate-zero-count-part2
  "Given a dial at start, return the number of times the dial passed zero based on the revolutions, direction, and clicks."
  [revolutions {start :start, :as command}]
  (if (zero? start)
    revolutions
    (let [did-cross-zero (would-cross-zero? command)]
      (if did-cross-zero (inc revolutions) revolutions))))

(defn- calculate-zero-count-part1
  "Given a dial at start, return 1 if the dials ends at 0 based on the direction and clicks. 0 otherwise"
  [_ command]
  (as-> (end-location command) end (if (zero? end) 1 0)))

(defn- calculate-zero-counts-and-dial-end-location
  "Process a dial input based on the start position of the dial and return how many times the dial passed zero and where the dial ended up"
  [{start :start, direction :direction, clicks :clicks} calculate-zero-count]
  (let [complete-revolutions (quot clicks 100)
        clicks-remain (rem clicks 100)
        command (->DialCommand start direction clicks-remain)
        end (end-location command)]
    {:zero-counts (calculate-zero-count complete-revolutions command),
     :dial-end end}))

(defn process
  [dial-start calculate-zero-count input-lines]
  (->> input-lines
       (map parse-directional-clicks)
       (reduce (fn [{dial-start :dial-end, zero-counts :zero-counts}
                    {direction :direction, clicks :clicks}]
                 (as-> (calculate-zero-counts-and-dial-end-location
                        (->DialCommand dial-start direction clicks)
                        calculate-zero-count)
                   result
                   {:zero-counts (+ zero-counts (:zero-counts result)),
                    :dial-end (:dial-end result)}))
               {:zero-counts 0, :dial-end dial-start})))

(with-open [rdr (io/reader (io/resource "day1/input.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process 50 calculate-zero-count-part1 input-lines))
        part-2 (time (process 50 calculate-zero-count-part2 input-lines))]
    (assert (= 5820 (:zero-counts part-2)) part-2)
    (assert (= 1007 (:zero-counts part-1)) part-1)))