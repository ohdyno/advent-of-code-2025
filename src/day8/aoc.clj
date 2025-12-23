(ns day8.aoc
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn process [input-lines])

;!zprint {:format :skip}
(let [input-lines
      ["162,817,812"
       "57,618,57"
       "906,360,560"
       "592,479,940"
       "352,342,300"
       "466,668,158"
       "542,29,236"
       "431,825,988"
       "739,650,466"
       "52,470,668"
       "216,146,977"
       "819,987,18"
       "117,168,530"
       "805,96,715"
       "346,949,466"
       "970,615,88"
       "941,993,340"
       "862,61,35"
       "984,92,344"
       "425,690,689"]
      ]
  (assert (test/is (= 40 (process input-lines)))))

(defn process-2 [input-lines])

(with-open [rdr (io/reader (io/resource "day8/input.txt"))]
  (let [input-lines (line-seq rdr)]
    (assert (test/is (= nil (time (process input-lines)))))
    (assert (test/is (= nil (time (process-2 input-lines)))))))