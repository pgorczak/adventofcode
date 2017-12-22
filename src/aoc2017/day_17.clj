(ns aoc2017.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn spinlock [step s i]
  (into [i] (->> s cycle (drop (inc step)) (take i))))

(defn short-circuit [step [after-0 pos] i]
  (let [insert-pos (rem (+ pos step) i)]
    (if (zero? insert-pos)
      [i 1]
      [after-0 (inc insert-pos)])))

(defn solve []
  (let [input (-> "2017-17" io/resource slurp str/trim (Integer/parseInt))]
    {:part-1 (-> (reduce (partial spinlock input) [0] (range 1 2018))
                 second)
     :part-2 (-> (reduce (partial short-circuit input) [1 1] (range 2 50e6))
                 first)}))
