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

(defn update-claim [old {id :id}]
  (if (some? old) (conj old id) #{id}))

(defn merge-claims [claims]
  (reduce (fn [acc c]
            (reduce (fn [acc xy] (update acc xy update-claim c))
                    acc
                    (unroll-claim c)))
          {}
          claims))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-3") (map parse-claim))
        claims (merge-claims input)]
    {:part-1 (->> claims vals (map count) (filter #(>= % 2)) count)
     :part-2 (difference
              (->> input (map :id) set)
              (->> claims vals (filter #(> (count %) 1)) (apply union)))}))
