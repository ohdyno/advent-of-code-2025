(ns aoc-day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defrecord ProductIdRange [start end])

(defn- parse-product-id-ranges
  [input-lines]
  (->> input-lines
       (map #(str/split % #","))
       (flatten)
       (map (fn [id-range]
              (let [[_ start end] (re-matches #"(\d+)-(\d+)" id-range)]
                (->ProductIdRange start end))))))

(def example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(parse-product-id-ranges [example])

(with-open [rdr (io/reader (io/resource "input-day-2.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (parse-product-id-ranges input-lines))]
    (doall part-1)))