(ns aoc2017.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [l]
  (let [[p1 p2 p3 v1 v2 v3 a1 a2 a3]
        (-> (str/trim l) (str/replace #"[<>\s[a-z]=]" "") (str/split #",")
            (as-> xs (map #(Integer/parseInt %) xs)))]
    {:p [p1 p2 p3] :v [v1 v2 v3] :a [a1 a2 a3]}))

(defn manhattan [xyz]
  (->> (map #(Math/abs %) xyz)
       (reduce +)))

(defn position [t {p :p v :v a :a}]
  (mapv + p (mapv #(* t %) v) (mapv #(/ (* t (inc t) %) 2) a)))

(defn step [{p :p v :v a :a}]
  {:p (mapv + p v a) :v (mapv + v a) :a a})

(defn drop-collisions [particles]
  (let [collisions (->> (map :p particles) frequencies (filter #(< 1 (val %)))
                        (map key) set)]
    (remove #(contains? collisions (:p %)) particles)))

(defn solve []
  (let [input (->> "2017-20" io/resource io/reader line-seq (map parse-line))]
    {:part-1 (->> input
                  (map-indexed (fn [i x] [i (manhattan (position 1e6 x))]))
                  (apply min-key second)
                  first)
     :part-2 (->> (iterate (comp drop-collisions #(map step %)) input)
                  (drop 1e3) first count)}))
