(ns aoc2020.day-03
  (:require [aoc-utils]))

(defn count-trees [lines right down]
  (let [width (count (first lines))]
    (->> (map #(mod (* right %) width) (range))
         (map nth (take-nth down lines))
         (filter #(= \# %))
         count)))

(defn solve []
  (let [input (aoc-utils/lines "2020-03")]
    {:part-1 (count-trees input 3 1)
     :part-2 (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
                  (map #(apply count-trees input %))
                  (apply *))}))
