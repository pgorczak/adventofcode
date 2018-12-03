(ns aoc2018.day-3
  (:require [aoc-utils]))

(defn parse-claim [s]
  (->> s
       (re-find #"^#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")
       rest
       (map #(Integer/parseInt %))
       (zipmap [:id :x :y :w :h])))

(defn unroll-claim [{:keys [x y w h]}]
  (for [ix (range x (+ x w))
        iy (range y (+ y h))]
    [ix iy]))

(defn count-overlaps [claims]
  (->> claims
       (map unroll-claim)
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-3") (map parse-claim))]
    {:part-1 (count-overlaps input)
     :part-2 nil}))
