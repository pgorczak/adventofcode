(ns aoc2018.day-7
  (:require [clojure.set :refer [difference]]
            [aoc-utils]))

(defn topo-reduce [f instrs]
  (loop [instrs instrs
         remaining (-> (apply concat instrs) set)
         acc {}]
    (if (empty? remaining)
      acc
      (let [blocked (-> (map second instrs) set)
            ready (difference remaining blocked)
            [acc done in-progress] (f acc ready)]
        (recur (remove #(contains? done (first %)) instrs)
               (difference remaining done in-progress)
               acc)))))

(defn steps [acc ready]
  (let [done (-> ready sort first)]
    [(update acc :order conj done)
     #{done}
     #{}]))

(defn step-time [s]
  (+ 61 (- (int s) (int \A))))

(defn update-all [f m]
  (zipmap (keys m) (map f (vals m))))

(defn process [acc ready]
  (let [working (get acc :working {})
        secs (get acc :secs 0)
        working (->> (take (- 5 (count working)) ready)
                     (map (fn [s] [s (step-time s)]))
                     (into working))
        [step done'] (->> (group-by val working) (apply min-key key))
        done (->> done' (map first) set)]
    [(assoc acc
            :working (->> (apply dissoc working done) (update-all #(- % step)))
            :secs (+ secs step))
     done
     (-> working keys set)]))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-7")
                   (map (fn [i] [(nth i 5) (nth i 36)])))]
    {:part-1 (->> (topo-reduce steps input) :order reverse (apply str))
     :part-2 (->> (topo-reduce process input) :secs)}))
