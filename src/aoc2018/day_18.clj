(ns aoc2018.day-18
  (:require [aoc-utils]))

(defn read-row [y s]
  (map-indexed (fn [x a] [[x y] a]) s))

(defn adjacent [state [x y]]
  (->> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (keep state)
       frequencies))

(defn evolve-one [state xy]
  (let [adj (adjacent state xy)]
    (case (state xy)
      \. (if (>= (adj \| 0) 3) \| \.)
      \| (if (>= (adj \# 0) 3) \# \|)
      \# (if (and (>= (adj \# 0) 1) (>= (adj \| 0) 1)) \# \.))))

(defn evolve [state]
  (->> (keys state)
       (map (fn [xy] [xy (evolve-one state xy)]))
       (into {})))

(defn resource-value [state]
  (let [fs (->> state vals frequencies)]
    (* (fs \# 0) (fs \| 0))))

(defn solve []
  (let [state (->> (aoc-utils/lines "2018-18")
                   (map-indexed read-row)
                   (apply concat)
                   (into {}))
        states (iterate evolve state)]
    {:part-1 (-> (nth states 10) resource-value)
     :part-2 nil}))
