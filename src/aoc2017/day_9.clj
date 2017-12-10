(ns aoc2017.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn char-seq [^java.io.Reader rdr]
   (let [chr (.read rdr)]
     (if (>= chr 0)
       (cons (char chr) (lazy-seq (char-seq rdr))))))

(def input-stream (-> "2017-9" io/resource io/reader char-seq))

(defn process [s]
  (loop [[c & r] s
         level 0
         garbage? false
         score 0
         g-count 0]
    (cond
      (nil? c) {:score score :garbage-count g-count}
      (= \! c) (recur (drop 1 r) level garbage? score g-count)
      garbage? (if (= c \>) (recur r level false score g-count)
                            (recur r level true score (inc g-count)))
      (= \< c) (recur r level true score g-count)
      (= \{ c) (recur r (inc level) garbage? score g-count)
      (= \} c) (recur r (dec level) garbage? (+ score level) g-count)
      :else (recur r level garbage? score g-count))))

(defn solve []
  (let [{p1 :score p2 :garbage-count} (process input-stream)]
    {:part-1 p1
     :part-2 p2}))
