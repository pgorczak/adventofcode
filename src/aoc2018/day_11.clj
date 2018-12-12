(ns aoc2018.day-11
  (:require [clojure.string :as str]
            [aoc-utils :refer [contents-trim]]))

(defn level [s [x y]]
  (let [id (+ x 10)]
    (-> id (* y) (+ s) (* id) str reverse (nth 2) str Integer/parseInt (- 5))))

(defn grid [s]
  (->> (for [y (range 300) x (range 300)] [x y])
       (mapv #(level s %))))

(defn row-get [n g [x y]]
  (let [o (-> (* 300 y) (+ x))]
    (->> (subvec g o (+ o n))
         (apply +))))

(defn rect-get [g w h [x y]]
  (->> (range y (+ y h))
       (map #(row-get w g [x %]))
       (apply +)))

(defn squares [g n]
  (let [max-i (- 300 (dec n))
        top-left (for [y (range max-i) x (range max-i)] [x y])]
    (zipmap top-left
            (map #(rect-get g n n %) top-left))))

(defn all-squares [g]
  (loop [[n & ns] (range 2 300)
         sqs (->> (squares g 1) (map (fn [[xy v]] [(conj xy 1) v])) (into {}))]
    (println n)
    (if (nil? n)
      sqs
      (let [max-i (- 300 (dec n))
            top-left (for [y (range max-i) x (range max-i)] [x y n])]
        (recur
         ns
         (->> top-left
              (map (fn [[x y n :as k]]
                     [k
                      (+ (get sqs [x y (dec n)])
                         (rect-get g n 1 [x (+ y (dec n))])
                         (rect-get g 1 (dec n) [(+ x (dec n)) y]))]))
              (into sqs)))))))

(defn max-coord [x]
  (->> x (apply max-key val) key (str/join \,)))

(defn solve []
  (let [input (-> (aoc-utils/contents-trim "2018-11") Integer/parseInt)
        g (grid input)]
    {:part-1 (max-coord (squares g 3))
     :part-2 (max-coord (all-squares g))}))
