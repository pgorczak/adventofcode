(ns aoc2017.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn step [[row col] [y x]]
  [(+ row y) (+ col x)])

(defn turn [tubes pos [y x]]
  (let [left [x (- y)]
        right [(- x) y]
        step-left (step pos left)]
    (if (contains? tubes step-left)
      [step-left left]
      [(step pos right) right])))

(defn sparsify [line]
  (keep-indexed (fn [col x] (if (not= \space x) [col x])) line))

(defn follow [tubes pos dir]
  (if-let [x (tubes pos)]
    (lazy-seq (cons x (if (= \+ x)
                          (let [[p d] (turn tubes pos dir)] (follow tubes p d))
                          (follow tubes (step pos dir) dir))))))

(defn solve []
  (let [input (->> "2017-19" io/resource io/reader line-seq (map sparsify)
                   (map-indexed (fn [r cs] (for [[c x] cs] [[r c] x])))
                   (apply concat) (into {}))
        start (->> input (filter #(-> % key first zero?)) ffirst)
        history (follow input start [1 0])]
    {:part-1 (->> history (filter #(Character/isLetter %)) str/join)
     :part-2 (count history)}))
