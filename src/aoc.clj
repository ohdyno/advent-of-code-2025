(ns aoc
  (:require [clojure.java.io :as io]))
(comment
  (remove-ns 'aoc))

(defn- parse-dial-input
  "Parse a single dial direction e.g. L68 into directional clicks e.g. {:direction :l, :clicks 68}"
  [input]
  (let [[_ _ direction clicks]
        (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)
        amount (Integer/parseInt clicks)]
    {:direction (keyword direction), :clicks amount}))

(defn- is-left? [direction] (= direction :L))

(defn- calculate-zero-count-part2
  [start end revolutions direction]
  (if (is-left? direction)
    (if (or (zero? start) (> end 0)) revolutions (inc revolutions))
    (if (or (zero? start) (<= end 99)) revolutions (inc revolutions))))

(defn- calculate-zero-count-part1 [_ end _ _] (if (zero? (mod end 100)) 1 0))

(defn- calculate-end
  [start direction clicks]
  (if (is-left? direction) (- start clicks) (+ start clicks)))

(defn- process-input
  "Process a dial input based on the start position of the dial and return how many times the dial passed zero and where the dial ended up"
  [start calculate-zero-count {direction :direction, clicks :clicks}]
  (let [complete-revolutions (quot clicks 100)
        clicks-remain (rem clicks 100)
        end (calculate-end start direction clicks-remain)]
    {:zero-counts
     (calculate-zero-count start end complete-revolutions direction),
     :dial-end (mod end 100)}))

(defn process-day-1
  [dial-start calculate-zero-count input-lines]
  (->> input-lines
       (map parse-dial-input)
       (reduce
        (fn [acc direction]
          (let [{additional-zero-counts :zero-counts, dial-end :dial-end}
                (process-input (:dial-end acc) calculate-zero-count direction)]
            (-> acc
                (update :zero-counts + additional-zero-counts)
                (assoc :dial-end dial-end))))
        {:zero-counts 0, :dial-end dial-start})))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process-day-1 50 calculate-zero-count-part1 input-lines))
        part-2 (time (process-day-1 50 calculate-zero-count-part2 input-lines))]
    (assert (= 5820 (:zero-counts part-2)))
    (assert (= 1007 (:zero-counts part-1)))))