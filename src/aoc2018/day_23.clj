(ns aoc2018.day-23
  (:require [aoc-utils]))

(defn read-bot [s]
  (->> (re-matches #"^pos=<([0-9-]+),([0-9-]+),([0-9-]+)>, r=([0-9]+)$" s)
       rest
       (map read-string)
       (zipmap [:x :y :z :r])))

(defn manhattan [b1 b2]
  (->> [:x :y :z]
       (map (fn [k] (Math/abs (- (b1 k) (b2 k)))))
       (apply +)))

(defn in-range? [b1 b2]
  (<= (manhattan b1 b2) (:r b1)))

(defn strongest [bots]
  (apply max-key :r bots))

(defn solve []
  (let [bots (->> (aoc-utils/lines "2018-23") (map read-bot))]
    {:part-1 (let [strongest-bot (strongest bots)]
               (->> (filter #(in-range? strongest-bot %) bots) count))
     :part-2 nil}))
