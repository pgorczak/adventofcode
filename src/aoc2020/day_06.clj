(ns aoc2020.day-06
  (:require [clojure.set]))

(defn parse-answers [i]
  (->> (clojure.string/split i #"\n\n")
       (map #(map set (clojure.string/split % #"\n")))))

(defn solve [input]
  (let [answers (-> input slurp parse-answers)]
    {:part-1 (->> answers
                  (map #(count (apply clojure.set/union %)))
                  (apply +))
     :part-2 (->> answers
                  (map #(count (apply clojure.set/intersection %)))
                  (apply +))}))
