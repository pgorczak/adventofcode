(ns aoc2019.day-6
  (:require [aoc-utils]))

;; builds a map of direct orbiters by planet
;; e.g. {[COM [B]], [B [C G]]}
(defn orbiters [map-entries]
  (loop [os {}
         [[a b] & entries] (map #(clojure.string/split % #"\)") map-entries)]
    (if (some? a)
      (recur (update os a (fnil #(conj % b) #{})) entries)
      os)))

;; turns direct orbiter map into a map of paths from COM to that orbit
;; e.g. {[COM []], [B [COM]], [C [COM B]], [G [COM B]]}
(defn orbit-paths [orbs]
  (loop [paths {}
         [[obj p] & todo] [["COM" []]]]
    (if (some? obj)
      (recur (assoc paths obj p)
             (->> (map vector (get orbs obj []) (repeat (conj p obj)))
                  (concat todo)))
      paths)))

;; Extract a path between two objects from the orbit-path map. Since the puzzle
;; only asks for the length, we could also just take a set-intersection of the
;; two paths and count that.
(defn transfer [paths from to]
  (let [p-from (paths from)
        p-to (paths to)
        n-common (->> (map vector p-from p-to)
                      (take-while (partial apply =))
                      count)]
    (concat (reverse (drop (dec n-common) p-from))
            (drop n-common p-to))))

(defn solve []
  (let [paths (-> (aoc-utils/lines "2019-6") orbiters orbit-paths)]
    {:part-1 (->> (vals paths) (map count) (reduce +))
     ;; a path with n stations takes n-1 transfers
     :part-2 (-> (transfer paths "YOU" "SAN") count dec)}))
