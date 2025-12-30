(ns day9.aoc
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn process [input-lines])
(defn process-2 [input-lines])

;!zprint {:format :skip}
(let [input-lines
      ["7,1"
       "11,1"
       "11,7"
       "9,7"
       "9,5"
       "2,5"
       "2,3"
       "7,3"]
      ]
  (assert (test/is (= 50 (process input-lines)))))

(with-open [rdr (io/reader (io/resource "day9/input.txt"))]
  (let [input-lines (line-seq rdr)]
    (assert (test/is (= nil (time (process input-lines)))))
    (assert (test/is (= nil (time (process-2 input-lines)))))))