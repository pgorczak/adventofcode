(ns aoc2017.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def hex-steps {:n [0 2] :nw [-1 1] :sw [-1 -1] :s [0 -2] :se [1 -1] :ne [1 1]})

(defn hex-step [[x1 y1] dir]
  (let [[x2 y2] (dir hex-steps)]
    [(+ x1 x2) (+ y1 y2)]))

(defn hex-distance [[x y]]
  (-> (+ (Math/abs x) (Math/abs y)) (/ 2)))

(defn solve []
  (let [input (-> "2017-11" io/resource slurp str/trim (str/split #",")
                  (as-> strings (map keyword strings)))
        path (reductions hex-step [0 0] input)]
    {:part-1 (-> path last hex-distance)
     :part-2 (->> path (map hex-distance) (apply max))}))
