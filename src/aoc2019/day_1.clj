(ns aoc2019.day-1
  (:require [aoc-utils]))

(defn fuel [mass]
  (-> mass (/ 3) int (- 2)))

(defn all-the-fuel [mass]
  (->> (iterate fuel mass) (drop 1) (take-while pos?) (reduce +)))

(defn solve []
  (let [input (map #(Integer/parseInt %) (aoc-utils/lines "2019-1"))]
    {:part-1 (->> input (map fuel) (reduce +))
     :part-2 (->> input (map all-the-fuel) (reduce +))}))
