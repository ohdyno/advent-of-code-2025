(ns day6.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn process
  [input-lines]
  (let [ops (->> (str/split (last input-lines) #"\s+")
                 (map #(if (= "*" %) [(resolve '*) 1] [(resolve '+) 0])))
        values (drop-last input-lines)]
    (->> (map #(str/split % #"\s+") values)
         (reduce (fn [acc v-str]
                   (->> (map bigint v-str)
                        (map (fn [[op acc] v] [op (op acc v)]) acc)))
                 ops)
         (map second)
         (reduce +))))

(let
  [input-lines
   (map
    str/trim
    (str/split-lines
     "123 328 51  64
      45  64  387 23
      6   98  215 314
      *   +   *   +"))]
  (assert (test/is (= 4277556N (process input-lines)))))

(with-open [rdr (io/reader (io/resource "day6/input.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines))
        part-2 nil]
    (assert (test/is (= 707 part-1)))
    (assert (test/is (= 361615643045059N part-2)))))