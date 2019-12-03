(ns aoc2019.day-3
  (:require [aoc-utils]
            [clojure.set]))

;; "U1,D1" -> [[:U 1] [:D 1]]
(defn parse-path [line]
  (for [segment (clojure.string/split line #",")]
    [(keyword (subs segment 0 1))
     (Integer/parseInt (subs segment 1))]))

(def directions
  {:R [ 1  0]
   :U [ 0  1]
   :L [-1  0]
   :D [ 0 -1]})

;; The trace-path function turns a parsed path into a list of [x y] locations
;; visited by the wire. Since lists grow by pre-pending, the final location
;; will be at the beginning of the list.

(defn trace-step [[[x y] & trace] [dx dy]]
  (conj trace [x y] [(+ x dx) (+ y dy)]))

(defn trace-path [path]
  (->> path
       (mapcat (fn [[dir n]] (repeat n (directions dir))))
       (reduce trace-step '([0 0]))))

(defn intersect [t1 t2]
  (-> (clojure.set/intersection (set t1) (set t2))
      (disj [0 0])))

;; Since traces are last-to-first location, the zipmap output will contain the
;; steps required to *first* get to a repeatedly visited location.

(defn step-intersect [t1 t2]
  (let [steps1 (zipmap t1 (-> t1 count range reverse))
        steps2 (zipmap t2 (-> t2 count range reverse))]
    (->> (intersect t1 t2)
         (map (juxt steps1 steps2)))))

(defn manhattan [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve []
  (let [input (->> (aoc-utils/lines "2019-3")
                   (map (comp trace-path parse-path)))]
    {:part-1 (->> (apply intersect input)
                  (map manhattan)
                  (apply min))
     :part-2 (->> (apply step-intersect input)
                  (map (partial apply +))
                  (apply min))}))
