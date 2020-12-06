(ns aoc2020.day-01
  (:require [aoc-utils]))

(defn combinations
  "Generates all combinations of k elements from vector v."
  [v k]
  (loop [combs []
         queues [v]]
    (cond
      (empty? (peek queues)) (if (= 1 (count queues))
                               combs
                               (recur combs (conj (-> queues pop pop)
                                                  (-> queues pop peek pop))))
      (< (count queues) k) (recur combs (->> queues peek pop (conj queues)))
      :else (recur (conj combs (map peek queues))
                   (->> queues peek pop (conj (pop queues)))))))

(defn sum-to [x v k]
  (->> (combinations v k)
       (filter #(= x (apply + %)))
       first))

(defn solve []
  (let [input (mapv #(Integer/parseInt %) (aoc-utils/lines "2020-01"))]
    {:part-1 (->> (sum-to 2020 input 2) (apply *))
     :part-2 (->> (sum-to 2020 input 3) (apply *))}))
