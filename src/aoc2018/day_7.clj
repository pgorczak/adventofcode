(ns aoc2018.day-7
  (:require [clojure.set :refer [difference]]
            [aoc-utils]))

(defn sort-steps [is]
  (loop [is is
         remaining (-> (apply concat is) set)
         ordered []]
    (if (empty? remaining)
      (apply str ordered)
      (let [blocked (-> (map second is) set)
            ready (-> (difference remaining blocked) sort first)]
        (recur (remove #(= ready (first %)) is)
               (disj remaining ready)
               (conj ordered ready))))))

(defn step-time [s]
  (+ 61 (- (int s) (int \A))))

(defn update-all [f m]
  (zipmap (keys m) (map f (vals m))))

(defn process [is]
  (loop [is is
         remaining (-> (apply concat is) set)
         working {}
         secs 0]
    (if (empty? remaining)
      secs
      (let [blocked (-> (map second is) set)
            ready (difference remaining blocked)
            working (->> (take (- 5 (count working)) ready)
                         (map (fn [s] [s (step-time s)]))
                         (into working))
            [step done'] (->> (group-by val working) (apply min-key key))
            done (->> done' (map first) set)]
        (recur (remove #(contains? done (first %)) is)
               (difference remaining (-> working keys set))
               (->> (apply dissoc working done) (update-all #(- % step)))
               (+ secs step))))))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-7")
                   (map (fn [i] [(nth i 5) (nth i 36)])))]
    {:part-1 (sort-steps input)
     :part-2 (process input)}))
