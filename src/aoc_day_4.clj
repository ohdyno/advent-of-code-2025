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
  "Return a map of roll location and the number of adjacent rolls."
  [rows cols rolls]
  (->> rolls
       (map (fn [[r c :as cell]]
              (let [adjacent-cells (generate-adjacent r c rows cols)]
                {cell (count (filter rolls adjacent-cells))})))))

(defn remove-rolls
  [rows cols cataloged]
  (let [adjacents (count-adjacent rows cols cataloged)
        removal-candidates (filter #(< (first (vals %)) 4) adjacents)]
    {:removed-count (count removal-candidates),
     :remaining (apply disj cataloged (mapcat keys removal-candidates))}))

(let [input-lines ["@@@" ".@@"]
      cataloged (catalog input-lines)
      rows (count input-lines)
      cols (count (first input-lines))]
  (remove-rolls rows cols cataloged))

(defn process
  ([input-lines opt]
   (let [cataloged (catalog input-lines)
         rows (count input-lines)
         cols (count (first input-lines))]
     (cond (= opt :once) (:removed-count (remove-rolls rows cols cataloged))
           :else (loop [total-removed 0
                        rolls-remaining cataloged]
                   (let [{removed :removed-count, remaining :remaining}
                         (remove-rolls rows cols rolls-remaining)]
                     (if (zero? removed)
                       total-removed
                       (recur (+ total-removed removed) remaining))))))))

(def example
  ["..@@.@@@@." "@@@.@.@.@@" "@@@@@.@.@@" "@.@@@@..@." "@@.@@@@.@@" ".@@@@@@@.@"
   ".@.@.@.@@@" "@.@@@.@@@@" ".@@@@@@@@." "@.@.@@@.@."])

(let [result (time (process example :once))]
  (assert (= 13 result))
  result)

(let [result (time (process example :repeat))] (assert (= 43 result) result))

(with-open [rdr (io/reader (io/resource "input-day-4.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process (vec input-lines) :once))
        part-2 (time (process (vec input-lines) :repeat))]
    (assert (= 1516 part-1) part-1)
    (assert (= 9122 part-2) part-2)))