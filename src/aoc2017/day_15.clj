(ns aoc2017.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (->> (str/split s #" ") last (Integer/parseInt)))

(defn make-generator [factor start]
  (->> start
       (iterate (fn [prev] (-> (* prev factor) (mod 2147483647))))
       (drop 1)))

(defn match?
  ([vs] (->> vs (map #(rem % 65536)) (apply =)))
  ([a b] (match? [a b])))

(defn multiple? [x y]
  (zero? (rem x y)))

(defn judge [as bs n]
  (->> (map match? as bs) (take n) (filter true?) count))

(def factor-a 16807)
(def factor-b 48271)

(defn solve []
  (let [input (->> "2017-15" io/resource io/reader line-seq
                   (map parse-line))
        [start-a start-b] input
        as (make-generator factor-a start-a)
        bs (make-generator factor-b start-b)]
    ; can't run both in one go (GC overhead limit exceeded)
    {:part-1 (judge as bs 40000000)
     :part-2 (judge (filter #(multiple? % 4) as)
                    (filter #(multiple? % 8) bs)
                    5000000)}))
