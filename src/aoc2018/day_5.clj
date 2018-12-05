(ns aoc2018.day-5
  (:require [aoc-utils]
            [clojure.string :refer [lower-case]]))

(defn react? [x y]
  (and (= (lower-case x) (lower-case y)) (not= x y)))

(defn react [p]
  (loop [left []
         [x & right] p]
    (cond
      (nil? right) (conj left x)
      (react? x (first right)) (if (empty? left)
                                 (recur left (rest right))
                                 (recur (pop left)
                                        (conj (rest right) (peek left))))
      :else (recur (conj left x) right))))

(defn solve []
  (let [input (aoc-utils/contents-trim "2018-5")]
    {:part-1 (-> input react count)
     :part-2 (->> input (map lower-case) set
                  (map (fn [u] (filter #(not= (lower-case %) u) input)))
                  (map react)
                  (map count)
                  (apply min))}))
