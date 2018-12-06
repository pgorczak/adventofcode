(ns aoc2018.day-6
  (:require [aoc-utils]))

(defn manhattan [[x y] [x' y']]
  (+ (Math/abs (- x x')) (Math/abs (- y y'))))

(defn rect-border [[x1 y1] [x2 y2]]
  (concat
   (for [x (range x1 (inc x2))] [x y1])
   (for [x (range x1 (inc x2))] [x y2])
   (for [y (range y1 (inc y2))] [x1 y])
   (for [y (range y1 (inc y2))] [x2 y])))

(defn rect-fill [[x1 y1] [x2 y2]]
  (for [x (range (inc x1) x2)
        y (range (inc y1) y2)]
    [x y]))

(defn closest [coords p]
  (->> coords
       (group-by #(manhattan p (val %)))
       (apply min-key key)
       val
       ((fn [c] (if (= 1 (count c)) (ffirst c))))))

(defn total [coords p]
  (->> (vals coords)
       (map (partial manhattan p))
       (reduce +)))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-6")
                   (map #(clojure.string/split % #", "))
                   (map (fn [xy] (map #(Integer/parseInt %) xy)))
                   (map-indexed vector)
                   (into {}))
        xs (map first (vals input))
        ys (map second (vals input))
        p1 [(apply min xs) (apply min ys)]
        p2 [(apply max xs) (apply max ys)]
        border (rect-border p1 p2)
        fill (rect-fill p1 p2)]
    {:part-1 (let [b (->> border (keep (partial closest input)) set)
                   f (->> fill (pmap (partial closest input))
                               (filter some?)
                               frequencies)]
               (->> (reduce dissoc f b) vals (apply max)))
     :part-2 (->> fill
                  (pmap (partial total input))
                  (filter #(< % 10000))
                  count)}))
