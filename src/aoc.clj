(ns aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(comment
  (remove-ns 'aoc))

(def dial-start 50)

(defrecord DialInput [direction clicks])

(defn parse-dial-input
  "Parse a single dial direction e.g. L68 into directional clicks e.g. {:direction :l, :clicks 68}"
  [input]
  (let [[_ _ direction clicks]
          (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)
        amount (Integer/parseInt clicks)]
    (->DialInput (keyword (str/lower-case direction)) amount)))

(defn- is-left? [direction] (= direction :l))

(defn- calculate-zero-count-part2
  [start end revolutions direction]
  (if (is-left? direction)
    (if (or (zero? start) (> end 0)) revolutions (inc revolutions))
    (if (or (zero? start) (<= end 99)) revolutions (inc revolutions))))

(defn- calculate-zero-count-part1 [_ end _ _] (if (zero? (mod end 100)) 1 0))

(defn- calculate-end
  [start direction clicks]
  (if (is-left? direction) (- start clicks) (+ start clicks)))

(defn process-input
  "Process a dial input based on the start position of the dial and return how many times the dial passed zero and where the dial ended up"
  [start calculate-zero-count {direction :direction, clicks :clicks}]
  (let [complete-revolutions (quot clicks 100)
        clicks-remain (rem clicks 100)
        end (calculate-end start direction clicks-remain)]
    {:zero-counts
       (calculate-zero-count start end complete-revolutions direction),
     :dial-end (mod end 100)}))

(defn process-day-1
  [calculate-zero-count input-lines]
  (->> input-lines
       (map parse-dial-input)
       (reduce (fn [acc direction]
                 (let [{additional-zero-counts :zero-counts, dial-end :dial-end}
                         (process-input (:dial-end acc)
                                        calculate-zero-count
                                        direction)]
                   (-> acc
                       (update :zero-counts + additional-zero-counts)
                       (assoc :dial-end dial-end))))
         {:zero-counts 0, :dial-end dial-start})))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process-day-1 calculate-zero-count-part1 input-lines))
        part-2 (time (process-day-1 calculate-zero-count-part2 input-lines))]
    (assert (= 5820 (:zero-counts part-2)))
    (assert (= 1007 (:zero-counts part-1)))))

(defn parse-directional-clicks
  "Parse a single dial direction e.g. L68 into directional clicks e.g. -68"
  [input]
  (let [{direction :direction, amount :clicks} (parse-dial-input input)]
    (if (= :l direction) (- amount) amount)))

(defn calculate-end-location
  [starting-at directional-clicks]
  (map-indexed (fn [index _]
                 (as-> index v
                   (inc v)
                   (take v directional-clicks)
                   (reduce + starting-at v)
                   (mod v 100)))
               directional-clicks))

(defn rotate-dial
  "Given a dial at dial-location, rotate the dial for the number of directional-clicks, and return the location at the end of the rotation"
  [dial-location directional-clicks]
  (-> dial-location
      (+ directional-clicks)
      (mod 100)))

(defn day-1-part-1-counter-increment-fn [_ dial-end _] (if (= 0 dial-end) 1 0))

(defn day-1-part-2-counter-increment-fn
  [dial-start _ number-clicks]
  (+ (if (= 0 dial-start) 1 0) (abs (quot number-clicks 100))))

(defn day-1-part-1-recursion
  [dial-initial turn-inputs counter-increment-fn]
  (loop [[directional-clicks & rest-of-directional-clicks]
           (map parse-directional-clicks turn-inputs)
         counter 0
         dial-locations [dial-initial]]
    (let [dial-start (last dial-locations)]
      (if directional-clicks
        (let [dial-end (rotate-dial dial-start directional-clicks)]
          (recur
            rest-of-directional-clicks
            (+ counter
               (counter-increment-fn dial-start dial-end directional-clicks))
            (conj dial-locations dial-end)))
        counter))))

(let [input-lines ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"]]
  (day-1-part-1-recursion dial-start
                          input-lines
                          day-1-part-2-counter-increment-fn))

(defn day-1-part-1-sequences
  [dial-start turn-inputs counter-increment-fn]
  (->> turn-inputs
       (map parse-directional-clicks)
       (calculate-end-location dial-start)
       (map #(counter-increment-fn nil % nil))
       (reduce +)))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (day-1-part-1-recursion
                       dial-start
                       input-lines
                       day-1-part-1-counter-increment-fn))]
    {:part-1 part-1}))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)]
    (time (day-1-part-1-sequences dial-start
                                  input-lines
                                  day-1-part-1-counter-increment-fn))))

