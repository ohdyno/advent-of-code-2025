(ns aoc
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]))
(comment
  "day 1, part 1"
  (repl/doc re-groups)
  (mod (+ dial-start -5) 99)
  (mod (+ dial-start (- 99 5)) 99)
  (+ dial-start 1))

(def dial-start 50)

(defn parse-dial-direction
  "Parse a single dial direction e.g. L68 into structured data e.g. {:direction 'L', :amount 68}"
  [input]
  (let [[_ _ direction amount]
        (re-matches #"((?<direction>[L|R])(?<amount>\d+))" input)]
    {:direction direction, :amount (Integer/parseInt amount)}))

(defn day-1-part-1
  [turn-inputs]
  (let [amount-with-direction (map
                               (fn [input]
                                 (let [{direction :direction, amount :amount}
                                       (parse-dial-direction input)]
                                   (if (= "L" direction) (- amount) amount)))
                               turn-inputs)
        dial-pointing-at (map-indexed (fn [i _]
                                        (reduce +
                                                dial-start
                                                (take (inc i) amount-with-direction)))
                                      amount-with-direction)]
    (count (filter #(= 0 (mod % 100)) dial-pointing-at))))

(with-open [rdr (io/reader (io/resource "input-day-1.txt"))]
  (day-1-part-1 (line-seq rdr)))