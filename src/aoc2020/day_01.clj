(ns aoc2020.day-01)

(defn combinations
  "Generates all combinations of k elements from vector v."
  [v k]
  (loop [combs []
         queues [v]]
    (cond
      (empty? (peek queues)) (if (= 1 (count queues))
                               combs
                               (recur combs (conj (-> queues pop pop)
                                                  (-> queues pop peek pop))))
      (< (count queues) k) (recur combs (->> queues peek pop (conj queues)))
      :else (recur (conj combs (map peek queues))
                   (->> queues peek pop (conj (pop queues)))))))

(defn sum-to [x v k]
  (->> (combinations v k)
       (filter #(= x (apply + %)))
       first))

(defn solve [input]
  (let [v (->> input line-seq (mapv #(Integer/parseInt %)))]
    {:part-1 (->> (sum-to 2020 v 2) (apply *))
     :part-2 (->> (sum-to 2020 v 3) (apply *))}))
