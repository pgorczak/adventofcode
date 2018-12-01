(ns aoc2018.day-1
  (:require [aoc-utils]))

(defn first-twice [input]
  (loop [freq 0
         seen #{}
         [change & rest] (cycle input)]
    (if (contains? seen freq)
      freq
      (recur (+ freq change) (conj seen freq) rest))))

(defn solve []
  (let [input (map #(Integer/parseInt %) (aoc-utils/lines "2018-1"))]
    {:part-1 (reduce + input)
     :part-2 (first-twice input)}))
