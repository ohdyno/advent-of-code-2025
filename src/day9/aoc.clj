(ns day9.aoc
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.pprint :as pprint]))

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

(defn is-vertical? [[a b]] (= (:x a) (:x b)))

(defn define-polygon
  [tiles]
  (conj (partition 2 1 tiles) [(last tiles) (first tiles)]))

(defn generate-boundary
  [[a b]]
  (->> (let [min-x (min (:x a) (:x b))
             max-x (max (:x a) (:x b))
             min-y (min (:y a) (:y b))
             max-y (max (:y a) (:y b))]
         [{:x min-x, :y min-y} {:x min-x, :y max-y} {:x max-x, :y max-y}
          {:x max-x, :y min-y}])
       (define-polygon)))

(defn do-segments-intersect?
  ([[[v1 v2] [h1 h2]]]
   {:pre [(= (:x v1) (:x v2)) (= (:y h1) (:y h2))]}
   (and (< (min (:x h1) (:x h2)) (:x v1) (max (:x h1) (:x h2)))
        (< (min (:y v1) (:y v2)) (:y h1) (max (:y v1) (:y v2)))))
  ([verticals horizontals]
   (->> (for [v verticals h horizontals] [v h])
        (some #(do-segments-intersect? %)))))

(defn calculate-segment-tiles
  [[a b]]
  (for [x (range (min (:x a) (:x b)) (inc (max (:x a) (:x b))))
        y (range (min (:y a) (:y b)) (inc (max (:y a) (:y b))))]
    {:x x, :y y}))

(defn calculate-boundary-tiles
  [polygon]
  (reduce #(into %1 (calculate-segment-tiles %2)) #{} polygon))

(defn calculate-neighbors
  [{x :x, y :y}]
  [{:x (dec x), :y (dec y)} {:x x, :y (dec y)} {:x (inc x), :y (dec y)}
   {:x (dec x), :y y} {:x (inc x), :y y} {:x (dec x), :y (inc y)}
   {:x x, :y (inc y)} {:x (inc x), :y (inc y)}])

(assert (test/is (= #{{:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 0, :y 1}
                      {:x 2, :y 1} {:x 0, :y 2} {:x 1, :y 2} {:x 2, :y 2}}
                    (set (calculate-neighbors {:x 1, :y 1})))))

(defn boundary-fill
  [boundary]
  (loop [[coordinate & remaining] [(-> (sort-by (juxt :x :y) boundary)
                                       (first)
                                       (update-vals inc))]
         inside #{}]
    (if (nil? coordinate)
      inside
      (let [[coords insides]
            (cond (inside coordinate) [remaining inside]
                  (boundary coordinate) [remaining inside]
                  :else [(into remaining (calculate-neighbors coordinate))
                         (conj inside coordinate)])]
        (recur coords insides)))))

(defn calculate-all-valid-tiles
  [polygon]
  (let [boundary (calculate-boundary-tiles polygon)
        inside (boundary-fill boundary)]
    (into inside boundary)))

(assert (test/is (empty? (set/difference
                          (set (for [x [0 1 2 3] y [0 1 2 3]] {:x x, :y y}))
                          (calculate-all-valid-tiles
                           (define-polygon [{:x 0, :y 0} {:x 3, :y 0}
                                            {:x 3, :y 3} {:x 0, :y 3}]))))))

(defn rectangle-contains-valid-tiles?
  [rectangle valid-tiles]
  (->> (generate-boundary rectangle)
       (calculate-boundary-tiles)
       (set/superset? valid-tiles)))

(defn process-2
  [input-lines]
  (let [tiles (parse-tiles input-lines)
        polygon (define-polygon tiles)
        valid-tiles (calculate-all-valid-tiles polygon)
        all-rectangles (sort-by :area > (build-rectangles tiles))]
    (-> (some
         #(if (rectangle-contains-valid-tiles? (:corners %) valid-tiles) % nil)
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

(comment
  (with-open [rdr (io/reader (io/resource "day9/input.txt"))]
    (let [input-lines (line-seq rdr)]
      (assert (test/is (= 4763040296N (time (process input-lines)))))
      (assert (test/is (= nil (time (process-2 input-lines))))))))