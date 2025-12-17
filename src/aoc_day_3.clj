(ns aoc-day-3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn prune
  [catalog selected invalid]
  (let [is-candidate? (comp not (set/union selected invalid))]
    (reduce-kv #(assoc %1 %2 (filter is-candidate? %3)) {} catalog)))

(defn select-index
  [idx exclusive-upper-idx remaining selected invalid]
  (let [bigger (range idx exclusive-upper-idx)
        smaller (range idx)
        bigger-contains-remaining?
        (>= (count (set/difference (set bigger) selected invalid)) remaining)]
    (if bigger-contains-remaining?
      [(conj selected idx) (into invalid smaller)]
      [(into selected bigger) invalid])))

(defn pick-batteries
  [bank batteries-needed catalog]
  (let [exclusive-upper-idx (count bank)]
    (loop [target-digit 9
           initial-catalog catalog
           selected #{}
           invalid #{}]
      (let [pruned-catalog (prune initial-catalog selected invalid)
            remaining (- batteries-needed (count selected))
            [idx] (pruned-catalog target-digit)]
        (cond (zero? remaining) selected
              (nil? idx) (recur (dec target-digit)
                                (dissoc pruned-catalog target-digit)
                                selected
                                invalid)
              :else (let [[updated-selected updated-invalid]
                          (select-index idx
                                        exclusive-upper-idx
                                        remaining
                                        selected
                                        invalid)]
                      (recur target-digit
                             pruned-catalog
                             updated-selected
                             updated-invalid)))))))


(defn- catalog-batteries
  [^String bank]
  (->> (vec bank)
       (reduce-kv (fn [m idx d]
                    (let [digit (^[char int] Character/digit d 10)]
                      (as-> (m digit []) $ (conj $ idx) (assoc m digit $))))
                  {})))

(defn combine-batteries
  [bank selected-indices]
  (->> (map-indexed vector bank)
       (filter #(selected-indices (first %)))
       (map second)
       (reduce str)))

(defn find-joltage
  [bank batteries-needed]
  (->> (catalog-batteries bank)
       (pick-batteries bank batteries-needed)
       (combine-batteries bank)))

(defn process
  [input batteries-needed]
  (->> (map #(find-joltage %1 batteries-needed) input)
       (reduce #(+ %1 (bigint %2)) 0)))

(comment
  (def example
    ["2213322222223222132231233322432224423222226232323522232215252332221122222231232224232722131522422232"])
  (bigint "123")
  (process example 12)
  (let [value (process example 2)] (assert (= 357 value) value)))

(with-open [rdr (io/reader (io/resource "input-day-3.txt"))]
  (let [input-lines (line-seq rdr)
        part-1 (time (process input-lines 2))
        part-2 (time (process input-lines 12))]
    (assert (= 17034 part-1) part-1)
    (assert (= 168798209663590N part-2) part-2)))