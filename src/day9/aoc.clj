(ns day9.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-tiles [input-lines]
  (map (fn [tile] (let [[x y] (str/split tile #",")]
                    {:x (Integer/parseInt x) :y (Integer/parseInt y)}
    )) input-lines)
  )

(defn calculate-areas [red-tiles]
  (for [a red-tiles b red-tiles
        :let [width (inc (Math/abs (double (- (:x a) (:x b)))))
              height (inc (Math/abs (double (- (:y a) (:y b)))))]]
    (bigint (* width height))))

(defn process [input-lines]
  (->> (parse-tiles input-lines)
       (calculate-areas)
       (sort >)
       (first)))

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
    (assert (test/is (= 4763040296N (time (process input-lines)))))
    (assert (test/is (= nil (time (process-2 input-lines)))))))