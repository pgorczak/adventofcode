(ns aoc2018.day-9
  (:require [aoc-utils :refer [contents-trim]]
            [clojure.zip :as z]))

(defn rotate [last first next z n]
  (loop [z z n n]
    (cond
      (zero? n) z
      (identical? z (last z)) (recur (first z) (dec n))
      :else (recur (next z) (dec n)))))

(def rotate-cw (partial rotate z/rightmost z/leftmost z/right))
(def rotate-ccw (partial rotate z/leftmost z/rightmost z/left))

(defn remove-rotate [z]
  (if (identical? z (z/leftmost z))
    (-> (z/remove z) z/down)
    (-> (z/remove z) (rotate-cw 1))))

(defn play-move [state [player marble]]
  (if (zero? (mod marble 23))
    (-> state
        (update :circle #(-> % (rotate-ccw 7) remove-rotate))
        (update-in [:scores player]
                   #(+ % marble (-> state :circle (rotate-ccw 7) z/node))))
    (-> state
        (update :circle #(-> (rotate-cw % 1)
                             (z/insert-right marble)
                             (rotate-cw 1))))))

(defn moves [n-players]
  (map vector (cycle (range 1 (inc n-players))) (drop 1 (range))))

(defn play-game [n-players n-marbles]
  (->> (reductions play-move
                   {:circle (z/down (z/vector-zip [0]))
                    :scores (zipmap (range 1 (inc n-players)) (repeat 0))}
                   (moves n-players))
       (take (inc n-marbles))
       last :scores vals
       (apply max)))

(defn solve []
  (let [input (->> (clojure.string/split (contents-trim "2018-9") #" ")
                   ((fn [ws] [(nth ws 0) (nth ws 6)]))
                   (map #(Integer/parseInt %)))]
    {:part-1 (apply play-game input)
     :part-2 nil}))
