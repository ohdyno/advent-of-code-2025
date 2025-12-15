(ns aoc-day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol InvalidIdGenerator
  (generate-invalid-ids [this is-invalid]
   "Generate invalid ids"))

(defn- calculate-digits [id] (count id))

(defn- has-even-digits
  [id]
  (-> (calculate-digits id)
      (even?)))

(defn- has-twice-repeated-sequence
  [id]
  (let [digits (calculate-digits id)
        divisor (quot digits 2)
        quotient (subs id 0 divisor)
        remainder (subs id divisor)]
    (= quotient remainder)))

(defn- is-invalid-part-1
  [id]
  (and (has-even-digits id) (has-twice-repeated-sequence id)))

(defn- is-invalid-part-2
  ([id]
   (loop [length 1
          max-length (quot (count id) 2)
          s id]
     (cond (> length max-length) false
           (is-invalid-part-2 id (subs id 0 length)) true
           :else (recur (inc length) max-length s))))
  ([id sub] (empty? (str/split id (re-pattern sub)))))

(defrecord ProductIdRange [start end]
  InvalidIdGenerator
    (generate-invalid-ids [_ is-invalid]
      (let [start-id (bigint start)
            end-id (bigint end)]
        (->> (range start-id (inc end-id))
             (map str)
             (filter is-invalid)))))

(defn- parse-product-id-ranges
  [input-lines]
  (->> input-lines
       (map #(str/split % #","))
       (flatten)
       (map (fn [id-range]
              (let [[_ start end] (re-matches #"(\d+)-(\d+)" id-range)]
                (->ProductIdRange start end))))))

(defn- process
  [input-lines is-invalid]
  (->> (parse-product-id-ranges input-lines)
       (map #(generate-invalid-ids % is-invalid))
       (flatten)
       (map bigint)
       (reduce +)))

(def example
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(let [result (process [example] is-invalid-part-1)]
  (assert (= 1227775554N result) result))

(let [result (process [example] is-invalid-part-2)]
  (assert (= 4174379265N result) result))

(with-open [rdr (io/reader (io/resource "input-day-2.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines is-invalid-part-1))
        part-2 (time (process input-lines is-invalid-part-2))]
    (assert (= 9188031749N part-1) part-1)
    (assert (= 11323661261N part-2) part-2)))