(ns aoc2018.day-13
  (:require [aoc-utils]))

(defn move [d [r c]]
  (case d
    \< [r (dec c)]
    \> [r (inc c)]
    \^ [(dec r) c]
    \v [(inc r) c]))

(def cw  {\< \^ \^ \> \> \v \v \<})
(def ccw {\< \v \v \> \> \^ \^ \<})
(def vert #{\^ \v})

(defn turn [t d ts]
  (case t
    \\ [(if (vert d) (ccw d) (cw d)) ts]
    \/ [(if (vert d) (cw d) (ccw d)) ts]
    \+ [((first ts) d) (rest ts)]
    [d ts]))

(defn read-layout [lines]
  (let [tracks
        (->> lines
             (map-indexed (fn [r l] (map-indexed (fn [c t] [[r c] t]) l)))
             (apply concat)
             (filter #(not= \space (second %))))]
    [(->> tracks
          (map (fn [[rc t]] [rc (case t \< \- \> \- \^ \| \v \| t)]))
          (into (sorted-map)))
     (->> tracks
          (keep (fn [[rc t]]
                  (if (#{\< \> \^ \v} t) [rc [t (cycle [ccw identity cw])]])))
          (into (sorted-map)))]))

(defn tick [layout carts]
  (loop [[[rc [d ts]] & carts] carts
         carts' (sorted-map)]
    (if (nil? rc)
      carts'
      (let [rc' (move d rc)
            dts' (turn (get layout rc') d ts)]
        (if (contains? carts' rc')
          (assoc carts' rc' [\X ts])
          (recur carts (assoc carts' rc' dts')))))))

(defn print-layout [layout carts]
  (let [carts (->> (for [[rc [d _]] carts] [rc d]) (into {}))]
    (doseq [r (range 6)]
      (->> (for [c (range 13)] [r c])
           (map (some-fn carts layout (constantly \space)))
           (apply str)
           (println)))
    (println)))

(defn without-turns [carts]
  (reduce-kv (fn [m k v] (assoc m k (first v))) (sorted-map) carts))

(defn collision-xy [states]
  (->> states
       (map without-turns)
       (map #(group-by val %))
       (drop-while #(not (contains? % \X)))
       first
       (#(get % \X))
       ffirst
       reverse
       (clojure.string/join ",")))

(defn solve []
  (let [input (aoc-utils/lines "2018-13")
        [layout carts] (read-layout input)
        states (iterate (partial tick layout) carts)]
    {:part-1 (collision-xy states)
     :part-2 nil}))
