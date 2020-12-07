(ns aoc2020.day-05
  (:require [clojure.set]))

(defn seat-id [s]
  (let [[row col] (split-at 7 s)]
    (->> [(map {\F 0 \B 1} row) (map {\L 0 \R 1} col)]
         (map #(-> (apply str %) (Integer/parseInt 2)))
         ((fn [[row col]] (-> (* 8 row) (+ col)))))))

(defn solve [input]
  (let [ids (->> input line-seq (map seat-id))
        min-id (apply min ids)
        max-id (apply max ids)]
    {:part-1 max-id
     :part-2 (clojure.set/difference (set (range min-id max-id)) (set ids))}))
