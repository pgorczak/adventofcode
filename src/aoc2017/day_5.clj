(ns aoc2017.day-5)

(defn exit-maze [offsets update-fn]
  (loop [offsets offsets
         index 0
         [count & counter] (range)]
    (if-let [jump (get offsets index)]
      (recur (update offsets index update-fn)
             (+ index jump)
             counter)
      count)))

(defn solve []
  (let [offsets (->> (clojure.java.io/resource "2017-5")
                     clojure.java.io/reader
                     line-seq
                     (map #(Integer/parseInt %))
                     (into []))]
    {:part-1 (exit-maze offsets inc)
     :part-2 (exit-maze offsets (fn [o] (if (>= o 3) (dec o) (inc o))))}))
