(ns aoc2019.day-4
  (:require [aoc-utils]))

(defn passwords [range-string]
  (let [[lo hi] (clojure.string/split range-string #"-")]
    (range (Integer/parseInt lo) (inc (Integer/parseInt hi)))))

;; 123 -> (1 2 3)
(defn digits [n]
  (loop [n n
         ds '()]
    (if (pos? n)
      (recur (quot n 10) (conj ds (mod n 10)))
      ds)))

(defn solve []
  (let [input (->> (aoc-utils/contents-trim "2019-4") passwords (map digits))
        runs (->> input
                  ;; keep only not-decreasing digits
                  (filter (partial apply <=))
                  ;; group digits into runs and count them
                  ;; (1 1 2 3) -> ((1 1) (2) (3)) -> (2 1 1)
                  (map #(->> (partition-by identity %) (map count))))]
    {:part-1 (->> runs (filter #(some (partial < 1) %)) count)
     :part-2 (->> runs (filter #(some (partial = 2) %)) count)}))
