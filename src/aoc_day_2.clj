(ns aoc-day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math :as math]))

(defprotocol InvalidIdGenerator
  (generate-invalid-ids [this]
   "Generate invalid ids"))

(defn- calculate-digits
  [id]
  (-> (math/log10 id)
      (math/floor)
      (inc)))

(defn- has-even-digits
  [id]
  (-> (calculate-digits id)
      (int)
      (even?)))

(defn- has-twice-repeated-sequence
  [id]
  (let [digits (calculate-digits id)
        divisor (math/pow 10 (quot digits 2))
        quotient (int (quot id divisor))
        remainder (int (rem id divisor))]
    (= quotient remainder)))

(defn- is-invalid
  [id]
  (and (has-even-digits id) (has-twice-repeated-sequence id)))

(defrecord ProductIdRange [start end]
  InvalidIdGenerator
    (generate-invalid-ids [_]
      (let [start-id (bigint start)
            end-id (bigint end)]
        (->> (range start-id (inc end-id))
             (filter is-invalid)))))

(generate-invalid-ids (->ProductIdRange "11" "1212"))

(defn- parse-product-id-ranges
  [input-lines]
  (->> input-lines
       (map #(str/split % #","))
       (flatten)
       (map (fn [id-range]
              (let [[_ start end] (re-matches #"(\d+)-(\d+)" id-range)]
                (->ProductIdRange start end))))))

(defn- process
  [input-lines]
  (->> (parse-product-id-ranges input-lines)
       (map generate-invalid-ids)
       (flatten)
       (reduce +)))

(def example
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(reduce + (flatten (process [example])))

(with-open [rdr (io/reader (io/resource "input-day-2.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines))]
    (assert (= 9188031749N part-1) part-1)))