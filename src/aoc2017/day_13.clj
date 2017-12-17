(ns aoc2017.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (let [[_ layer depth] (re-find #"(\d+):\s(\d+)" s)]
    [(Integer/parseInt layer) (Integer/parseInt depth)]))

(defn interval [depth]
  (if (= 1 depth)
    1
    (* 2 (dec depth))))

(defn severity [range depth]
  (if (->> (interval depth) (mod range) zero?)
    (* range depth)
    0))

(defn path-severity [delay firewalls]
  (->> firewalls
       (map (fn [[range depth]] (severity (+ range delay) depth)))
       (apply +)))

(defn solve []
  (let [input (->> "2017-13" io/resource io/reader line-seq
                   (map parse-line))]
    {:part-1 (path-severity 0 input)
     :part-2 (->> (range)
                  (map (fn [x] [x (path-severity x input)]))
                  (drop-while #(-> % second pos?))
                  ffirst)}))
