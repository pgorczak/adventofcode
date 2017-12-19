(ns aoc2017.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (->> (str/split s #" ") last (Integer/parseInt)))

(defn match? [a b]
  (= (bit-and a 0xFFFF) (bit-and b 0xFFFF)))

(defn judge [as bs n]
  (->> (map match? as bs) (take n) (filter true?) count))

(defn gen-a [x] (rem (* x 16807) 2147483647))
(defn gen-b [x] (rem (* x 48271) 2147483647))
(defn gen [gen-fn x0] (drop 1 (iterate gen-fn x0)))
(defn gen-multiples [gen-fn x0 a] (filter #(zero? (rem % a)) (gen gen-fn x0)))

(defn solve []
  (let [[a0 b0] (->> "2017-15" io/resource io/reader line-seq (map parse-line))]
    {:part-1 (judge (gen gen-a a0) (gen gen-b b0) 40e6)
     :part-2 (judge (gen-multiples gen-a a0 4)
                    (gen-multiples gen-b b0 8)
                    5e6)}))
