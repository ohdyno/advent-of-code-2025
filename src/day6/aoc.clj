(ns day6.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn process
  [input-lines]
  (let [ops (->> (str/split (last input-lines) #"\s+")
                 (map #(if (= "*" %) [(resolve '*) 1] [(resolve '+) 0])))
        values (drop-last input-lines)]
    (->> (map #(str/split % #"\s+") values)
         (reduce (fn [acc v-str]
                   (->> (map bigint v-str)
                        (map (fn [[op acc] v] [op (op acc v)]) acc)))
                 ops)
         (map second)
         (reduce +))))

(defn extract-numbers
  [max-lengths input-line]
  (loop [line input-line
         [length & remaining-length] max-lengths
         numbers []]
    (if (nil? length)
      numbers
      (recur (drop (inc length) line)
             remaining-length
             (conj numbers (take length line))))))

(defn find-max-length-per-column
  [values]
  (->> (map #(str/split (str/trim %) #"\s+") values)
       (map #(map count %))
       (reduce #(map max %1 %2))))

(defn process-2
  [input-lines]
  (let [ops (->> (str/split (last input-lines) #"\s+")
                 (map #(if (= "*" %) (resolve '*) (resolve '+))))
        values (drop-last input-lines)
        max-lengths (find-max-length-per-column values)]
    (->> (map #(extract-numbers max-lengths %) values)
         (apply map vector)
         (map #(apply map vector %))
         (map #(map (comp bigint str/trim str/join) %))
         (map #(apply %1 %2) ops)
         (reduce +))))

(let [input-lines ["123 328  51 64 " " 45 64  387 23 " "  6 98  215 314"
                   "*   +   *   +  "]]
  (assert (test/is (= 3263827N (process-2 input-lines)))))

(with-open [rdr (io/reader (io/resource "day6/input.txt"))]
  (let [input-lines (line-seq rdr)]
    (assert (test/is (= 4412382293768N (time (process input-lines)))))
    (assert (test/is (= 7858808482092N (time (process-2 input-lines)))))))