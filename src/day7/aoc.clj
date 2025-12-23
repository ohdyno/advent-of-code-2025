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




(defn find-beam-splits
  "Given a graph representing where the beams have been and where the splitters are located, return all the locations where the beams need to split."
  [beams level splitters]
  (filter (fn [[lvl idx]] (and (splitters idx) (= lvl (dec level))))
          (keys beams)))

(defn split-beams
  "Update the beams locations with the result of splitting the beam at all the splitter locations on current level.
  If splitters is a number, then the beams will be split at [level splitters]."
  [beams [level splitters :as split]]
  (cond (coll? splitters)
        (let [splits (find-beam-splits beams level splitters)]
          (reduce (fn [beams split] (split-beams beams split)) beams splits))
        :else (let [left [(inc level) (dec splitters)]
                    right [(inc level) (inc splitters)]]
                (assoc beams split #{left right} left #{} right #{}))))

(defn build-beam-traverse-graph
  [beam splitters]
  (reduce (fn [graph [_ splitters-per-level :as split]]
            (if (empty? splitters-per-level) graph (split-beams graph split)))
          {[0 beam] #{}}
          splitters))

(defn process-2
  [input-lines]
  (let [trimmed (filter #(< 1 (count (set %))) input-lines)
        beam (locate-beam (first trimmed))]
    (->> (map locate-splitters trimmed)
         (map-indexed vector)
         (build-beam-traverse-graph beam))))

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
       "...............";1
       ".......^.......";2
       "...............";3
       "......^.^......";4
       "...............";5
       ".....^.^.^.....";6
       "...............";7
       "....^.^...^....";8
       "...............";9
       "...^.^...^.^...";10
       "...............";11
       "..^...^.....^..";12
       "...............";13
       ".^.^.^.^.^...^.";14
       "...............";15
       ]
      ]
  (as-> (process input-lines) result
        (assert (test/is (= 21 result)))
        (process-2 input-lines)
        ))

(comment
  (with-open [rdr (io/reader (io/resource "day7/input.txt"))]
    (let [input-lines (line-seq rdr)] (time (process-2 input-lines)))))