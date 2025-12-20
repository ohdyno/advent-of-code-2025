(ns aoc-day-4
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn catalog
  [grid]
  (->> grid
       (map-indexed (fn [row-number row]
                      (map-indexed #(hash-map :row row-number
                                              :col %1
                                              :is-roll (if (= %2 \@) :roll nil))
                                   row)))
       (reduce into)
       (reduce
        (fn [m {row :row, col :col, is-role :is-roll}]
          (update m is-role #(if (nil? %1) #{[row col]} (conj %1 [row col]))))
        {})))

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
  (->> (:roll cataloged)
       (count-adjacent rows cols)
       (map #(first (vals %)))
       (filter #(< %1 4))))

(defn process
  [input-lines]
  (let [cataloged (catalog input-lines)
        rows (count input-lines)
        cols (count (first input-lines))]
    (->> (:roll cataloged)
         (count-adjacent rows cols)
         (map #(first (vals %)))
         (filter #(< %1 4))
         (count))))

(def example
  ["..@@.@@@@." "@@@.@.@.@@" "@@@@@.@.@@" "@.@@@@..@." "@@.@@@@.@@" ".@@@@@@@.@"
   ".@.@.@.@@@" "@.@@@.@@@@" ".@@@@@@@@." "@.@.@@@.@."])

(assert (= 13 (time (process example))))

(comment
  (with-open [rdr (io/reader (io/resource "input-day-4.txt"))]
    (let [input-lines (line-seq rdr)
          part-1 (time (process (vec input-lines)))
          part-2 nil]
      (assert (= 1516 part-1) part-1))))