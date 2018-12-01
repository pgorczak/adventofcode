(ns aoc2018.day-1
  (:require [aoc-utils]))

(defn first-twice [input]
  (loop [[freq & rest] (->> input cycle (reductions + 0))
         seen #{}]
    (if (seen freq)
      freq
      (recur rest (conj seen freq)))))

(defn solve []
  (let [input (map #(Integer/parseInt %) (aoc-utils/lines "2018-1"))]
    {:part-1 (reduce + input)
     :part-2 (first-twice input)}))
