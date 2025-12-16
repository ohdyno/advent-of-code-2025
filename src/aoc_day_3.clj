(ns aoc-day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  ["987654321111111" "811111111111119" "234234234234278" "818181911112111"])

(defn pick
  [bank batteries-needed largest-digits]
  (let [[[largest-index largest-digit] & others] largest-digits
        length (count bank)]
    (if (= 1 (- length largest-index))
      (let [[_ second-largest-digit] (first others)]
        [second-largest-digit largest-digit])
      (let [[[_ largest-after]]
            (take 1 (filter (fn [[index]] (> index largest-index)) others))]
        [largest-digit largest-after]))))


(defn- indexed-digits-comparator
  [[a-idx a] [b-idx b]]
  (let [digit-compare (compare b a)
        index-compare (compare a-idx b-idx)]
    (if (zero? digit-compare) index-compare digit-compare)))

(defn find-joltage
  [bank batteries-needed]
  (->> (map-indexed vector bank)
       (sort indexed-digits-comparator)
       (pick bank batteries-needed)))

(defn process
  [input batteries-needed]
  (->> (map #(find-joltage %1 batteries-needed) input)
       (map str/join)
       (reduce #(+ %1 (Integer/parseInt %2)) 0)))

(let [value (process example 2)] (assert (= 357 value) value))

(with-open [rdr (io/reader (io/resource "input-day-3.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines 2))
        part-2 []]
    (assert (= 17034 part-1) part-1)
    (assert (= [] part-2) part-2)))
