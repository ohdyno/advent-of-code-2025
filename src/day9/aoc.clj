(ns day9.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-tiles
  [input-lines]
  (mapv (fn [tile]
          (let [[x y] (str/split tile #",")]
            [(Integer/parseInt x) (Integer/parseInt y)]))
        input-lines))

(defn build-rectangles
  [red-tiles]
  (for [[x y :as a] red-tiles
        [x' y' :as b] red-tiles
        :let [width (inc (Math/abs (double (- x x'))))
              height (inc (Math/abs (double (- y y'))))]]
    {:corners [a b], :area (bigint (* width height))}))

(defn process
  [input-lines]
  (->> (parse-tiles input-lines)
       (build-rectangles)
       (sort-by :area >)
       (first)
       (:area)))

(defn define-polygon
  [tiles]
  (let [closed (conj tiles (first tiles))] (map vector closed (rest closed))))

(defn miss?
  [rectangle line]
  (let [[[r1x r1y] [r2x r2y]] rectangle
        [[p1x p1y] [p2x p2y]] line
        missed (or (<= (max p1x p2x) (min r1x r2x))
                   (<= (max r1x r2x) (min p1x p2x))
                   (<= (max p1y p2y) (min r1y r2y))
                   (<= (max r1y r2y) (min p1y p2y)))]
    missed))

(defn inside? [rectangle polygon] (every? #(miss? rectangle %) polygon))

(defn process-2
  [input-lines]
  (let [tiles (parse-tiles input-lines)
        polygon (define-polygon tiles)
        rectangles (sort-by :area > (build-rectangles tiles))]
    (->> rectangles
         (some #(when (inside? (:corners %) polygon) (:area %))))))

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
  (assert (test/is (= 24 (process-2 input-lines)))))

(comment
  (with-open [rdr (io/reader (io/resource "day9/input.txt"))]
    (let [input-lines (line-seq rdr)]
      (assert (test/is (= 4763040296N (time (process input-lines)))))
      (assert (test/is (= 1396494456N (time (process-2 input-lines))))))))