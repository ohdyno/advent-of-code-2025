(ns day9.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-tiles
  [input-lines]
  (map (fn [tile]
         (let [[x y] (str/split tile #",")]
           {:x (Integer/parseInt x), :y (Integer/parseInt y)}))
       input-lines))

(defn build-rectangles
  [red-tiles]
  (for [a red-tiles
        b red-tiles
        :let [width (inc (Math/abs (double (- (:x a) (:x b)))))
              height (inc (Math/abs (double (- (:y a) (:y b)))))]]
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
  (conj (partition 2 1 tiles) [(last tiles) (first tiles)]))

(defn generate-coordinates
  [[a b]]
  (for [x (range (min (:x a) (:x b)) (inc (max (:x a) (:x b))))
        y (range (min (:y a) (:y b)) (inc (max (:y a) (:y b))))]
    {:x x, :y y}))

(defn does-ray-cross-segment?
  "Return true if the ray starting at coordinate and going downwards crosses the segment defined by the points a and b."
  [coordinate [a b]]
  (->> (< (min (:x a) (:x b)) (:x coordinate) (max (:x a) (:x b)))
       (and (< (:y coordinate) (max (:y a) (:y b))))))

(defn is-coordinate-within-polygon?
  [coordinate polygon]
  (->> (map #(does-ray-cross-segment? coordinate %) polygon)
       (filter true?)
       (count)
       (odd?)))

(defn is-vertical? [[a b]] (= (:x a) (:x b)))

(defn is-coordinate-on-segment?
  [coordinate [a b :as segment]]
  (if (is-vertical? segment)
    (and (= (:x coordinate) (:x a))
         (<= (min (:y a) (:y b)) (:y coordinate) (max (:y a) (:y b))))
    (and (= (:y coordinate) (:y a))
         (<= (min (:x a) (:x b)) (:x coordinate) (max (:x a) (:x b))))))

(defn is-coordinate-on-polygon?
  [coordinate polygon]
  (some #(is-coordinate-on-segment? coordinate %) polygon))

(defn is-coordinate-within-or-on-polygon?
  [coordinate polygon]
  (or (is-coordinate-on-polygon? coordinate polygon)
      (is-coordinate-within-polygon? coordinate polygon)))

;!zprint {:format :skip}
(defn is-rectangle-within-polygon
  [rectangle polygon]
  (->> (generate-coordinates (:corners rectangle))
       (every? #(is-coordinate-within-or-on-polygon? % polygon))))

(let [input-lines ["7,1" "11,1" "11,7" "9,7" "9,5" "2,5" "2,3" "7,3"]]
  (let [tiles (parse-tiles input-lines)
        polygon (define-polygon tiles)
        all-rectangles (sort-by :area > (build-rectangles tiles))]
    (is-coordinate-within-or-on-polygon? {:x 11, :y 1} polygon)))

(defn process-2
  [input-lines]
  (let [tiles (parse-tiles input-lines)
        polygon (define-polygon tiles)
        all-rectangles (sort-by :area > (build-rectangles tiles))]
    (-> (some #(if (is-rectangle-within-polygon % polygon) % nil)
              all-rectangles)
        (:area))))

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

(with-open [rdr (io/reader (io/resource "day9/input.txt"))]
  (let [input-lines (line-seq rdr)]
    (assert (test/is (= 4763040296N (time (process input-lines)))))
    (assert (test/is (= nil (time (process-2 input-lines)))))))