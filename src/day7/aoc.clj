(ns day7.aoc
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]
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

(defn build-beam-traverse-graph
  [beam splitters]
  (->> splitters
       (filter not-empty)
       (map-indexed vector)
       (reduce
        (fn [graph [level splitters-per-level]]
          (let [beams-to-split (filter (fn [m]
                                         (let [[k v] (first m)]
                                           (and (splitters-per-level v)
                                                (= k level))))
                                       graph)]
            (reduce-kv
             (fn [m k v]
               (let [lvl (inc (first k))
                     idx (second k)]
                 (merge
                  {[lvl (dec idx)] #{}, [lvl (inc idx)] #{}}
                  (assoc m k (into v [[lvl (dec idx)] [lvl (inc idx)]])))))
             graph
             beams-to-split)))
        {[0 beam] #{}})))

(defn count-timelines
  ([graph]
   (let [[start] (sort (keys graph))] (first (count-timelines graph {} start))))
  ([graph memo node]
   (if-let [memo-value (memo node)]
     [memo-value memo]
     (if-let [edges (not-empty (graph node))]
       (let [[count-left memo-left] (count-timelines graph memo (first edges))]
         (if-let [right (second edges)]
           (let [[count-right memo-right]
                 (count-timelines graph memo-left right)
                 result (+ count-left count-right)]
             [result (assoc memo-right node result)])
           [count-left (assoc memo-left node count-left)]))
       [1 (assoc memo node 1)]))))

(defn process-2
  [input-lines]
  (let [[beam-line & manifolds] input-lines
        beam (locate-beam beam-line)]
    (->> (map locate-splitters manifolds)
         (build-beam-traverse-graph beam)
         (count-timelines))))

;!zprint {:format :skip}
(let [input-lines
      [".......S......."
       ".......^......."
       "......^.^......"
       ".....^.^.^....."
       "....^.^...^...."
       ]
      ]
  (assert (test/is (= 13 (process-2 input-lines)))))

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