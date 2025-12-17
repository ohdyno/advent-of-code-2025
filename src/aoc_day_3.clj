(ns aoc-day-3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn prune
  [locations selected invalid]
  (reduce-kv
   (fn [m digit idxs]
     (assoc m digit (filter #(not (or (selected %) (invalid %))) idxs)))
   {}
   locations))

(defn pick-batteries
  [bank batteries-needed digit-locations]
  (let [length (count bank)]
    (loop [target-digit 9
           locations digit-locations
           selected #{}
           invalid #{}]
      (let [pruned-locations (prune locations selected invalid)
            remaining (- batteries-needed (count selected))
            [idx] (pruned-locations target-digit)]
        (cond (zero? remaining) selected ; found the correct number of
                                         ; batteries
              (< target-digit 0) (do (println bank) {})
              (nil? idx) (recur (dec target-digit)
                                (dissoc pruned-locations target-digit)
                                selected
                                invalid) ; no more <target-digit> in the
                                         ; eligible locations
              :else (let [selected-with-index (conj selected idx)]
                      (if (>= (count (set/difference
                                      (into #{} (range (inc idx) length))
                                      selected
                                      invalid))
                              remaining)
                        (recur target-digit
                               pruned-locations
                               selected-with-index
                               (into invalid (range idx)))
                        (recur target-digit
                               pruned-locations
                               (into selected-with-index
                                     (range (inc idx) length))
                               invalid))))))))


(defn- extract-digit-locations
  [^String bank]
  (->> (map-indexed vector bank)
       (reduce (fn [m [idx d]]
                 (let [digit (^[char int] Character/digit d 10)]
                   (->> idx
                        (conj (m digit []))
                        (assoc m digit))))
               {})))

(defn combine-batteries
  [bank selected-indices]
  (reduce str
          (map second
               (filter #(selected-indices (first %))
                       (map-indexed vector bank)))))

(defn find-joltage
  [bank batteries-needed]
  (->> (extract-digit-locations bank)
       (pick-batteries bank batteries-needed)
       (combine-batteries bank)))

(defn process
  [input batteries-needed]
  (->> (map #(find-joltage %1 batteries-needed) input)
       (map str/join)
       (reduce #(+ %1 (bigint %2)) 0)))

(comment
  (def example
    ["2213322222223222132231233322432224423222226232323522232215252332221122222231232224232722131522422232"])
  (bigint "123")
  (process example 12)
  (let [value (process example 2)] (assert (= 357 value) value))
  (with-open [rdr (io/reader (io/resource "input-day-3.txt"))]
    (let [input-lines (line-seq rdr)
          part-1 (time (process input-lines 2))
          part-2 (time (process input-lines 12))]
      (assert (= 17034 part-1) part-1)
      (assert (= 168798209663590N part-2) part-2))))
