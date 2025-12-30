(ns day8.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-junction-boxes
  [input-lines]
  (map (fn [coords]
         (let [[x y z] (str/split coords #",")]
           {:x (Double/parseDouble x),
            :y (Double/parseDouble y),
            :z (Double/parseDouble z)}))
       input-lines))

(defn calculate-distance
  [p q]
  (let [xs (Math/pow (- (:x p) (:x q)) 2)
        ys (Math/pow (- (:y p) (:y q)) 2)
        zs (Math/pow (- (:z p) (:z q)) 2)]
    (Math/sqrt (+ xs ys zs))))

(defn calculate-distances
  [junction-boxes]
  (->> (for [p junction-boxes q junction-boxes :when (not= p q)] [p q])
       (reduce (fn [m [p q :as pair]]
                 (let [key (sort-by (juxt :x :y :z) pair)
                       distance (calculate-distance p q)]
                   (assoc m key distance)))
               {})))

(defn connect-to-circuits
  [circuits [p q]]
  (let [contains-p-or-q? (group-by (fn [circuit]
                                     (or (contains? circuit p)
                                         (contains? circuit q)))
                                   circuits)
        contains-pq (contains-p-or-q? true)
        does-not-contain-pq (contains-p-or-q? false)
        pq-circuit (conj (reduce #(into %1 %2) #{} contains-pq) p q)]
    (conj does-not-contain-pq pq-circuit)))

(defn build-circuits
  [boxes-with-distances]
  (->> (sort-by second boxes-with-distances)
       (take 1000)
       (map first)
       (reduce connect-to-circuits [])))

(defn process
  [input-lines]
  (->> (parse-junction-boxes input-lines)
       (calculate-distances)
       (build-circuits)
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

;!zprint {:format :skip}
(defn process-2 [input-lines])

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
  (assert (test/is (= 25272 (process-2 input-lines)))))

(comment
  (with-open [rdr (io/reader (io/resource "day8/input.txt"))]
    (let [input-lines (line-seq rdr)]
      (assert (test/is (= 123234 (time (process input-lines)))))
      (assert (test/is (= nil (time (process-2 input-lines))))))))