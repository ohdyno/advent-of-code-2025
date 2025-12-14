(ns aoc
  (:require [clojure.java.io :as io]))
(comment
  (remove-ns 'aoc))

(defrecord DialCommand [start direction clicks])

(defn- parse-directional-clicks
  "Parse a single dial direction e.g. L68 into directional clicks e.g. {:direction :l, :clicks 68}"
  [input]
  (let [[_ _ direction clicks]
        (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)
        amount (Integer/parseInt clicks)]
    {:direction (keyword direction), :clicks amount}))

(defn- is-left? [direction] (= direction :L))

(defn- did-cross-zero?
  "Given a dial at start, return if the dial had crossed zero based on the direction and clicks."
  [{start :start, direction :direction, clicks :clicks}]
  (if (is-left? direction) (>= clicks start) (>= clicks (- 100 start))))

(defn- calculate-zero-count-part2
  "Given a dial at start, return the number of times the dial passed zero based on the revolutions, direction, and clicks."
  [revolutions {start :start, :as command}]
  (if (zero? start)
    revolutions
    (let [did-cross-zero (did-cross-zero? command)]
      (if did-cross-zero (inc revolutions) revolutions))))

(defn- calculate-end
  "Given a dial at start, calculate where the dial ends based on the direction and the number of clicks"
  [{start :start, direction :direction, clicks :clicks}]
  (-> (if (is-left? direction) (- start clicks) (+ start clicks))
      (mod 100)))

(defn- calculate-zero-count-part1
  "Given a dial at start, return 1 if the dials ends at 0 based on the direction and clicks. 0 otherwise"
  [_ command]
  (as-> (calculate-end command) end (if (zero? end) 1 0)))

(defn- calculate-zero-counts-and-dial-end-location
  "Process a dial input based on the start position of the dial and return how many times the dial passed zero and where the dial ended up"
  [{start :start, direction :direction, clicks :clicks} calculate-zero-count]
  (let [complete-revolutions (quot clicks 100)
        clicks-remain (rem clicks 100)
        command (->DialCommand start direction clicks-remain)
        end (calculate-end command)]
    {:zero-counts (calculate-zero-count complete-revolutions command),
     :dial-end end}))

(defn process-day-1
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

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process-day-1 50 calculate-zero-count-part1 input-lines))
        part-2 (time (process-day-1 50 calculate-zero-count-part2 input-lines))]
    (assert (= 5820 (:zero-counts part-2)) part-2)
    (assert (= 1007 (:zero-counts part-1)))))