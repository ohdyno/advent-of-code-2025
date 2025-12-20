(ns aoc-day-4
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn catalog
  [grid]
  (->> (for [[row-number row] (map-indexed vector grid)
             [col-number c] (map-indexed vector row)]
         {:row row-number, :col col-number, :is-roll (if (= c \@) :roll nil)})
       (group-by :is-roll)
       (:roll)
       (reduce (fn [s {r :row, c :col}] (conj s [r c])) #{})))

(defn generate-adjacent
  [r c rows cols]
  (->> (combo/cartesian-product (range (dec r) (+ r 2)) (range (dec c) (+ c 2)))
       (filter (fn [[r' c']] (not (and (= r r') (= c c')))))
       (filter (fn [[r' c']]
                 (and (<= 0 r') (< r' rows) (<= 0 c') (< c' cols))))))

(defn count-adjacent
  [rows cols rolls]
  (->> rolls
       (map (fn [[r c :as cell]]
              (let [adjacent-cells (generate-adjacent r c rows cols)]
                {cell (count (filter rolls adjacent-cells))})))))

(let [input-lines ["@@" "@@"]
      cataloged (catalog input-lines)
      rows (count input-lines)
      cols (count (first input-lines))]
  (->> cataloged
       (count-adjacent rows cols)
       (map #(first (vals %)))
       (filter #(< %1 4))))

(defn process
  [input-lines]
  (let [cataloged (catalog input-lines)
        rows (count input-lines)
        cols (count (first input-lines))]
    (->> cataloged
         (count-adjacent rows cols)
         (map #(first (vals %)))
         (filter #(< %1 4))
         (count))))

(def example
  ["..@@.@@@@." "@@@.@.@.@@" "@@@@@.@.@@" "@.@@@@..@." "@@.@@@@.@@" ".@@@@@@@.@"
   ".@.@.@.@@@" "@.@@@.@@@@" ".@@@@@@@@." "@.@.@@@.@."])

(let [result (time (process example))]
  (assert (= 13 result))
  result)

(comment
  (with-open [rdr (io/reader (io/resource "input-day-4.txt"))]
    (let [input-lines (line-seq rdr)
          part-1 (time (process (vec input-lines)))
          part-2 nil]
      (assert (= 1516 part-1) part-1))))