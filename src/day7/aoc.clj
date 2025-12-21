(ns day7.aoc
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn locate-beam [line] (str/index-of line \S))

(defn locate-splitters
  [line]
  (->> (map-indexed vector line)
       (filter (comp (partial = \^) second))
       (map first)
       set))

(defn calculate-beam-splits
  [beams splits]
  (->> (mapcat #(vector (dec %) (inc %)) splits)
       (into (set/difference beams splits))))

(defn process
  [input-lines]
  (let [beam (locate-beam (first input-lines))
        manifolds (drop 1 input-lines)]
    (loop [beams #{beam}
           [splitters & remaining] (map locate-splitters manifolds)
           split-count 0]
      (if (nil? splitters)
        split-count
        (let [splits (set/intersection beams splitters)
              beam-splits (calculate-beam-splits beams splits)]
          (recur beam-splits remaining (+ (count splits) split-count)))))))

(defn process-2 [input-lines])

;!zprint {:format :skip}
(let [input-lines
      [".......S......."
       "..............."
       ".......^......."
       "..............."
       "......^.^......"
       "..............."
       ".....^.^.^....."
       "..............."
       "....^.^...^...."
       "..............."
       "...^.^...^.^..."
       "..............."
       "..^...^.....^.."
       "..............."
       ".^.^.^.^.^...^."
       "..............."]
      ]
  (as-> (process input-lines) result
        (assert (test/is (= 21 result)))))

(with-open [rdr (io/reader (io/resource "day7/input.txt"))]
  (let [input-lines (line-seq rdr)]
    (assert (test/is (= 1678 (time (process input-lines)))))
    (assert (test/is (= nil (time (process-2 input-lines)))))))