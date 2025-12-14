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

(defn process-input
  "Process a dial input based on the start position of the dial and return how many times the dial passed zero and where the dial ended up"
  [start {direction :direction, clicks :clicks}]
  (let [complete-revolutions (quot clicks 100)
        clicks-remain (rem clicks 100)]
    (if (is-left? direction)
      (let [end (- start clicks-remain)]
        {:zero-counts (if (or (zero? start) (> end 0))
                        complete-revolutions
                        (inc complete-revolutions)),
         :dial-end (mod end 100)})
      (let [end (+ start clicks-remain)]
        {:zero-counts (if (or (zero? start) (<= end 99))
                        complete-revolutions
                        (inc complete-revolutions)),
         :dial-end (mod end 100)}))))

(process-input 0 {:direction :l, :clicks 1250})

(defn foo
  [input-lines]
  (->> input-lines
       (map parse-dial-input)
       (reduce (fn [{zero-counts :zero-counts, dial-end :dial-end} direction]
                 (let [{n :zero-counts, de :dial-end} (process-input dial-end
                                                                     direction)]
                   {:zero-counts (+ n zero-counts), :dial-end de}))
         {:zero-counts 0, :dial-end dial-start})))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)] (time (foo input-lines))))


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

(defn day-1-part-2 [turn-inputs])

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (day-1-part-1-recursion dial-start
                                             input-lines
                                             day-1-part-1-counter-increment-fn))
        part-2 (time (day-1-part-1-recursion
                       dial-start
                       input-lines
                       day-1-part-2-counter-increment-fn))]
    {:part-1 part-1, :part-2 part-2}))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (let [input-lines (line-seq rdr)]
    (time (day-1-part-1-sequences dial-start
                                  input-lines
                                  day-1-part-1-counter-increment-fn))))

