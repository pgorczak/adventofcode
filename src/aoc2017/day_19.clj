(ns aoc2017.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn step [dir [row col]]
  (case dir
    :up [(dec row) col]
    :down [(inc row) col]
    :left [row (dec col)]
    :right [row (inc col)]))

(defn turn [tubes dir pos]
  (case dir
    (:up :down)
    (let [l (step :left pos)]
      (if (contains? tubes l) [l :left] [(step :right pos) :right]))
    (:left :right)
    (let [u (step :up pos)]
      (if (contains? tubes u) [u :up] [(step :down pos) :down]))))

(defn sparsify [line]
  (keep-indexed (fn [col x] (if (not= \space x) [col x])) line))

(defn follow [tubes pos dir]
  (if-let [x (tubes pos)]
    (lazy-seq (cons x (if (= \+ x)
                          (let [[p d] (turn tubes dir pos)] (follow tubes p d))
                          (follow tubes (step dir pos) dir))))))

(defn solve []
  (let [input (->> "2017-19" io/resource io/reader line-seq (map sparsify)
                   (map-indexed (fn [r cs] (for [[c x] cs] [[r c] x])))
                   (apply concat) (into {}))
        start (->> input (filter #(-> % key first zero?)) ffirst)
        history (follow input start :down)]
    {:part-1 (->> history (filter #(Character/isLetter %)) str/join)
     :part-2 (count history)}))
