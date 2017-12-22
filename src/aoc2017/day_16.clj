(ns aoc2017.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2017.day-6 :refer [until-repeat]]))

(def init "abcdefghijklmnop")
(def n-programs 16)

(defn spin [x s]
  (->> s cycle (drop (- n-programs x)) (take n-programs)))

(defn exchange [x y s]
  (let [split1 (min x y)
        split2 (max x y)
        [a [b & s2]] (split-at split1 s)
        [c [d & e]] (split-at (- split2 split1 1) s2)]
    (concat a [d] c [b] e)))

(defn partner [x y [e & s]]
  (if (some? e)
    (lazy-seq (cons (cond (= e x) y (= e y) x :else e) (partner x y s)))))

(defn parse-move [[type & args]]
  (let [args (str/join args)
        [arg1 arg2] (str/split args #"/")]
    (case type
      \s (partial spin (Integer/parseInt args))
      \x (partial exchange (Integer/parseInt arg1) (Integer/parseInt arg2))
      \p (partial partner (first arg1) (first arg2)))))

(defn solve []
  (let [input (-> "2017-16" io/resource slurp str/trim (str/split #","))
        moves (map parse-move input)
        dance (fn [s] (-> (reduce #(%2 %1) s moves) str/join))]
    {:part-1 (dance init)
     :part-2 (let [dances (iterate dance init)
                   period (-> dances until-repeat :seen count)]
               (nth dances (rem 1e9 period)))}))
