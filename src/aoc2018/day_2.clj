(ns aoc2018.day-2
  (:require [aoc-utils]))

(defn checksum [input]
  (->> input
       (map (comp distinct vals frequencies))
       (apply concat)
       frequencies
       ((fn [counts] (* (get counts 2 0) (get counts 3 0))))))

(defn hamming [x y]
  (->> (map vector x y)
       (filter (partial apply not=))
       count))

(defn common-str [x y]
  (->> (map vector x y)
       (filter (partial apply =))
       (map first)
       (apply str)))

(defn pairs [x]
  (loop [[e & r :as s] (set x)
         ps '()]
    (if r
      (recur (disj s e)
             (->> r (map #(vec [e %])) (concat ps)))
      ps)))

(defn find-common-str [input]
  (->> (pairs input)
       (filter #(= 1 (apply hamming %)))
       first
       (apply common-str)))

(defn solve []
  (let [input (aoc-utils/lines "2018-2")]
    {:part-1 (checksum input)
     :part-2 (find-common-str input)}))
