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
  (let [beams-passed-through (set/difference beams splits)
        split-beams (mapcat #(vector (dec %) (inc %)) splits)]
    (into beams-passed-through split-beams)))

(defn process
  [input-lines]
  (let [[beam-line & manifolds] input-lines
        beam (locate-beam beam-line)]
    (->> (map locate-splitters manifolds)
         (reduce (fn [[beams split-count] splitters]
                   (let [splits (set/intersection beams splitters)]
                     [(calculate-beam-splits beams splits)
                      (+ (count splits) split-count)]))
                 [#{beam} 0])
         second)))

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