(ns aoc2018.day-2
  (:require [aoc-utils]))

(defn checksum [input]
  (->> input
       (map (comp distinct vals frequencies))
       (apply concat)
       frequencies
       ((fn [counts] (* (get counts 2 0) (get counts 3 0))))))

(defn solve []
  (let [input (aoc-utils/lines "2018-2")]
    {:part-1 (checksum input)
     :part-2 nil}))
