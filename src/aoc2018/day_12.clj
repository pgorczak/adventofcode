(ns aoc2018.day-12
  (:require [aoc-utils]))

(defn plant-indexes [s]
  (->> s (keep-indexed #(if (= %2 \#) %1)) set))

(defn initial-state [s]
  (-> s (clojure.string/split #": ") second plant-indexes))

(defn grow-rules [s]
  (->> s (filter #(= \# (nth % 9)))
       (map #(subs % 0 5))
       (map plant-indexes)
       (map #(-> (for [i %] (- i 2)) set))))

(defn grow? [rules state i]
  (->> (range (- i 2) (+ i 3))
       (keep state)
       (map #(- % i))
       set
       ((fn [s] (some #(= s %) rules)))))

(defn evolve [rules state]
  (->> (range (- (apply min state) 3) (+ (apply max state) 4))
       (filter #(grow? rules state %))
       set))

(defn solve []
  (let [input (aoc-utils/lines "2018-12")
        init (initial-state (first input))
        rules (grow-rules (drop 2 input))
        states (->> (iterate #(evolve rules %) init))]
    {:part-1 (->> (nth states 20) (apply +))
     :part-2 nil}))
