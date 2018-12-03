(ns aoc2018.day-3
  (:require [clojure.set :refer [difference union]]
            [aoc-utils]))

(defn parse-claim [s]
  (->> s
       (re-find #"^#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")
       rest
       (map #(Integer/parseInt %))
       (zipmap [:id :x :y :w :h])))

(defn unroll-claim [{:keys [x y w h]}]
  (for [ix (range x (+ x w))
        iy (range y (+ y h))]
    [ix iy]))

(defn count-overlaps [claims]
  (->> claims
       (map unroll-claim)
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count))

(defn overlap? [{x1 :x y1 :y w1 :w h1 :h} {x2 :x y2 :y w2 :w h2 :h}]
  (and (< x1 (+ x2 w2)) (> (+ x1 w1) x2)
       (< y1 (+ y2 h2)) (> (+ y1 h1) y2)))

(defn intact-claims [claims]
  (loop [[c & next-claims] claims
         overlaps #{}
         intacts #{}]
    (if-not c
      intacts
      (let [new-overlaps (->> intacts (filter (partial overlap? c)) set)
            c-overlap? (or (not-empty new-overlaps)
                           (some (partial overlap? c) overlaps))
            next-overlaps (union overlaps new-overlaps)
            next-intacts (difference intacts new-overlaps)]
        (if c-overlap?
          (recur next-claims (conj next-overlaps c) next-intacts)
          (recur next-claims next-overlaps (conj next-intacts c)))))))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-3") (map parse-claim))]
    {:part-1 (count-overlaps input)
     :part-2 (-> (intact-claims input) first :id)}))
