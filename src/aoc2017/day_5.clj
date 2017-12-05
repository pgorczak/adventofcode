(ns aoc2017.day-5)

(def input (slurp (clojure.java.io/resource "2017-5")))
(def offsets (->> input clojure.string/split-lines (map #(Integer/parseInt %))))

(defn exit-maze [seq update-fn]
  (let [offsets (into [] seq)
        n-offsets (count offsets)
        outside? (fn [idx] (or (neg? idx) (>= idx n-offsets)))]
    (loop [off offsets idx 0 ctr 0]
      (if (outside? idx)
        ctr
        (recur (update off idx update-fn) (+ idx (get off idx)) (inc ctr))))))

{:part1 (exit-maze offsets inc)
 :part2 (exit-maze offsets (fn [o] (if (>= o 3) (dec o) (inc o))))}
