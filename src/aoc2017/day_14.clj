(ns aoc2017.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2017.day-10 :refer [knot-hash]]
            [aoc2017.day-12 :refer [cluster-groups]]))

(def grid-indexes
  (for [row (range 128)
        col (range 128)]
    [row col]))

(defn hex->grid [s]
  (->> (Integer/parseInt (str s) 16)
       Integer/toBinaryString
       (format "%4s")
       (replace {\space false \0 false \1 true})))

(defn sparsify [grid]
  (->> (flatten grid)
       (map (fn [i used?] (if used? i)) grid-indexes)
       (filter some?)
       (into #{})))

(defn neighborhood [sparse-grid [row col]]
  (->> [[row (inc col)] [row (dec col)] [(inc row) col] [(dec row) col]]
       (filter #(contains? sparse-grid %))
       (into [])))

(defn pipe-ify [sparse-grid]
  (->> sparse-grid
       (map (fn [x] [x {:pipes (neighborhood sparse-grid x)}]))
       (into {})))

(defn solve []
  (let [input (->> "2017-14" io/resource slurp str/trim)
        grid (->> (range 128)
                  (map #(str input "-" %))
                  (map knot-hash)
                  (map #(mapcat hex->grid %)))]
    {:part-1 (->> grid flatten (filter true?) count)
     :part-2 (->> grid sparsify pipe-ify cluster-groups count)}))
