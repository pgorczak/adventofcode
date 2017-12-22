(ns aoc2017.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def n-marks 256)

(defn braid
  ([] {:hash (range n-marks) :pos 0 :skip 0})
  ([lengths] (braid (braid) lengths))
  ([{hash :hash pos :pos skip :skip} [length & lengths]]
   (if (nil? length)
     {:hash hash :pos pos :skip skip}
     (let [[taken not-taken] (->> hash cycle (drop pos) (split-at length))
           after-pos (concat (reverse taken)
                             (->> not-taken (take (- n-marks length))))
           [part-2 part-1] (->> after-pos (split-at (- n-marks pos)))]
       (recur {:hash (concat part-1 part-2)
               :pos (mod (+ pos length skip) n-marks)
               :skip (inc skip)}
              lengths)))))

(defn knot-hash [s]
  (->> (concat (map int s) [17 31 73 47 23])
       (repeat 64)
       (reduce braid (braid))
       :hash
       (partition 16)
       (map #(apply bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(defn solve []
  (let [input (-> "2017-10" io/resource slurp str/trim)]
    {:part-1 (->> (str/split input #",")
                  (map #(Integer/parseInt %))
                  braid
                  :hash
                  (take 2)
                  (apply *))
     :part-2 (knot-hash input)}))
