(ns aoc-day-4
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.math.combinatorics :as combo]))

(defn catalog
  [input-lines]
  (reduce (fn [m {row :row, col :col, is-role :is-roll}]
            (update m is-role #(if (nil? %1) #{[row, col]} (conj %1 [row, col]))))
          {}
          (reduce into
                  (map-indexed (fn [row-number row]
                                 (map-indexed (fn [column-number c]
                                                {:row row-number,
                                                 :col column-number,
                                                 :is-roll (keyword (if (= c \@)
                                                                     :roll
                                                                     nil))})
                                              row))
                               input-lines))))

(defn foo
  [rows cols rolls neighbors [r c]]
  (let [neighbors-to-update (filter (fn [[row col :as cell]] (and (not (and (= r row) (= c col)))
                                                         (spec/int-in-range? 0 rows row)
                                                         (spec/int-in-range? 0 cols col)
                                                         (some #{cell} rolls))) (combo/cartesian-product (range (dec r) (+ r 2))
                                                                                                                        (range (dec c) (+ c 2))))]
    (reduce (fn [m cell] (update m cell #(if (nil? %1) 1 (inc %1)))) neighbors neighbors-to-update)
    )
  )

(defn count-neighbors
  [rows cols cataloged]
  (->> (:roll cataloged)
       (reduce #(foo rows cols (:roll cataloged) %1 %2) {})))

(let [input-lines [".@@"
                   "@.@"
                   "@@@"]
      cataloged (catalog input-lines)
      rows (count input-lines)
      cols (count (first input-lines))]
  (->> cataloged
       ))

(defn process
  [input-lines]
  (let [cataloged (catalog input-lines)
        rows (count input-lines)
        cols (count (first input-lines))]
    (->> cataloged
         )))

(def example
  ["..@@.@@@@."
  "@@@.@.@.@@"
  "@@@@@.@.@@"
  "@.@@@@..@."
  "@@.@@@@.@@"
  ".@@@@@@@.@"
   ".@.@.@.@@@"
   "@.@@@.@@@@"
   ".@@@@@@@@."
   "@.@.@@@.@."])

(time (process example))

(assert (= 13 (process example)))

(comment
  (with-open [rdr (io/reader (io/resource "input-day-4.txt"))]
    (let [input-lines (line-seq rdr)
          part-1 (time (process (vec input-lines)))
          part-2 nil]
      (assert (= 1512 part-1) part-1)
      (assert (= nil part-2)))))