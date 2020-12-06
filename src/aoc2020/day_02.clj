(ns aoc2020.day-02
  (:require [aoc-utils]))

(defn parse-entry [l]
  (let [[_ lo hi ch pw] (re-find #"(\d+)-(\d+)\ ([a-z]):\ ([a-z]+)" l)]
    {:lo (Integer/parseInt lo) :hi (Integer/parseInt hi) :c (first ch) :pw pw}))

(defn valid-1? [{:keys [lo hi c pw]}]
  (<= lo (count (filter #(= c %) pw)) hi))

(defn valid-2? [{:keys [lo hi c pw]}]
  (let [pos1 (= c (nth pw (dec lo)))
        pos2 (= c (nth pw (dec hi)))]
    (and (or pos1 pos2) (not (and pos1 pos2)))))

(defn solve []
  (let [input (map parse-entry (aoc-utils/lines "2020-02"))]
    {:part-1 (->> input (filter valid-1?) count)
     :part-2 (->> input (filter valid-2?) count)}))
