(ns aoc2020.day-10)

(defn arrangement-counts [adapters]
  (reduce (fn [paths a]
            (->> paths
                 (take-while #(<= (- a (key %)) 3))
                 (map val)
                 (apply +)
                 (assoc paths a)))
          (sorted-map-by > (first adapters) 1)
          (rest adapters)))

(defn solve [input]
  (let [adapters (->> input line-seq (map #(Integer/parseInt %)) sort)
        adapters (-> (into [0] adapters) (conj (+ 3 (last adapters))))]
    {:part-1 (->> adapters
                  (#(map vector (rest %) %))
                  (map #(apply - %))
                  frequencies
                  (#(* (% 1) (% 3))))
     :part-2 (-> (arrangement-counts adapters)
                 (get (last adapters)))}))
