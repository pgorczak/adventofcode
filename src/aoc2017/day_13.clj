(ns aoc2017.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (let [[_ layer depth] (re-find #"(\d+):\s(\d+)" s)]
    [(Integer/parseInt layer) (Integer/parseInt depth)]))

(defn severity [range depth]
  (if (->> (* 2 (dec depth)) (mod range) zero?)
    (* range depth)
    0))

(defn severities [delay firewalls]
  (map (fn [[range depth]] (severity (+ range delay) depth)) firewalls))

(defn solve []
  (let [input (->> "2017-13" io/resource io/reader line-seq
                   (map parse-line))]
    {:part-1 (->> (severities 0 input) (apply +))
     :part-2 (->> (range)
                  (map (fn [x] [x (severities x input)]))
                  (filter #(->> % second (every? zero?)))
                  ffirst)}))
