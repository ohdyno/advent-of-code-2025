(ns day7.aoc
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.pprint :as pprint]
            [clojure.walk]))

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

(defn start-beam [line] (map #(if (= \S %) 1 0) line))

(defn split-beams'
  [beams line]
  (let [lookup (into {} (map-indexed vector beams))]
    (->> (vec line)
         (reduce-kv (fn [m idx c]
                      (if (= c \.)
                        (assoc m idx (+ (m idx 0) (lookup idx)))
                        (assoc m
                               (dec idx)
                               (+ (m (dec idx)) (lookup idx))
                               idx
                               0
                               (inc idx)
                               (lookup idx))))
                    {})
         (sort-by first)
         (reduce (fn [acc [_ v]] (conj acc v)) []))))

(assert (test/is (= [1] (split-beams' [1] (vec ".")))))
(assert (test/is (= [1 0 1] (split-beams' [0 1 0] (vec ".^.")))))
(assert (test/is (= [2 0 1] (split-beams' [1 1 0] (vec ".^.")))))
(assert (test/is (= [1 0 2] (split-beams' [0 1 1] (vec ".^.")))))
(assert (test/is (= [0 1 0 2 0 1 0]
                    (split-beams' [0 0 1 0 1 0 0] (vec "..^.^..")))))
(assert (test/is (= [0 1 0 3 0 2 0]
                    (split-beams' [0 0 1 0 2 0 0] (vec "..^.^..")))))

(defn trace-beams
  [beams line]
  (if (empty? beams) (start-beam line) (split-beams' (last beams) line)))

(defn process-2
  [input-lines]
  (->> input-lines
       (reduce (fn [beams line] [(trace-beams beams line)]) [])
       (flatten)
       (reduce +)))

;!zprint {:format :skip}
(let [input-lines
      ["....S...."                                    ;0
       "........."
       "....^...."                                    ;1
       "........."
       "...^.^..."                                    ;2
       "........."
       "..^.^.^.."                                    ;3
       "........."
       ".^.^...^."                                    ;4
       "........."
       ]
      ]
  (process-2 input-lines))

;!zprint {:format :skip}
(let [input-lines
      [".......S.......";0
       "..............."
       ".......^.......";1
       "..............."
       "......^.^......";2
       "..............."
       ".....^.^.^.....";3
       "..............."
       "....^.^...^....";4
       "..............."
       "...^.^...^.^...";5
       "..............."
       "..^...^.....^..";6
       "..............."
       ".^.^.^.^.^...^.";7
       "..............."
       ]
      ]
  (as-> (process input-lines) result
        (assert (test/is (= 21 result)))
        (process-2 input-lines)
        (assert (test/is (= 40 result)))))

(comment
  (with-open [rdr (io/reader (io/resource "day7/input.txt"))]
    (let [input-lines (line-seq rdr)] (time (process-2 input-lines)))))