(ns day5.aoc
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

(defn overlaps?
  [range-window id-range]
  (>= (second range-window) (first id-range)))

(defn merge-range
  [range-window id-range]
  [(first range-window) (max (second range-window) (second id-range))])

(defn merge-overlapping
  [fresh-sorted]
  (loop [range-window (first fresh-sorted)
         non-overlapping []
         [id-range & remaining] (rest fresh-sorted)]
    (if (nil? id-range)
      (conj non-overlapping range-window)
      (let [[range-window' non-overlapping']
            (if (overlaps? range-window id-range)
              [(merge-range range-window id-range) non-overlapping]
              [id-range (conj non-overlapping range-window)])]
        (recur range-window' non-overlapping' remaining)))))

(defn process-part-2
  [input-lines]
  (let [{fresh :fresh-id-range} (parse-inventory input-lines)
        fresh-sorted (sort-by (juxt first) fresh)]
    (reduce +
            (map (fn [[lower upper]] (- (inc upper) lower))
                 (merge-overlapping fresh-sorted)))))

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

(let [result (process-part-2 example)] (assert (test/is (= 14 result))))

(let [result (process example)] (assert (test/is (= 3 result))))

(with-open [rdr (io/reader (io/resource "day5/input.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines))
        part-2 (time (process-part-2 input-lines))]
    (assert (test/is (= 707 part-1)))
    (assert (test/is (= 361615643045059N part-2)))))