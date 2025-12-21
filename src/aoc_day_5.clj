(ns aoc-day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-inventory
  [input-lines]
  (reduce (fn [m line]
            (let [id-range (re-matches #"(\d+)-(\d+)" line)]
              (cond id-range (->> (map bigint (rest id-range))
                                  (update m :fresh-id-range conj))
                    (empty? line) m
                    :else (update m :available-ids conj (bigint line)))))
          {:fresh-id-range [], :available-ids []}
          input-lines))

(defn process
  [input-lines]
  (let [{fresh :fresh-id-range, ids :available-ids} (parse-inventory
                                                     input-lines)]
    (->> (map (fn [id] (some (fn [[lower upper]] (<= lower id upper)) fresh))
              ids)
         (filter true?)
         (count))))

(def example
  (map
   str/trim
   (str/split-lines
    "
        3-5
        10-14
        16-20
        12-18

        1
        5
        8
        11
        17
        32")))

(process example)

(let [result (process example)] (assert (test/is (= 3 result))))

(with-open [rdr (io/reader (io/resource "input-day-5.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines))]
    (assert (test/is (= 707 part-1)))))