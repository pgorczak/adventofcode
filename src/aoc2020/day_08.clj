(ns aoc2020.day-08)

(defn parse-instruction [s]
  (let [[i n] (clojure.string/split s #" ")]
    [(keyword i) (Integer/parseInt n)]))

(defn run
  ([program]
   (iterate #(run program %) {:pc 0 :acc 0}))
  ([program state]
   (when-let [[i n] (program (state :pc))]
     (case i
       :nop (update state :pc inc)
       :acc (-> state (update :pc inc) (update :acc + n))
       :jmp (update state :pc + n)))))

(defn run-til-end [program]
  (reduce (fn [seen {:keys [pc acc]}]
            (cond
              (= pc (count program)) (reduced [:term acc])
              (seen pc) (reduced [:loop acc])
              :else (conj seen pc)))
          #{}
          (run program)))

(defn variations [program]
  (keep (fn [[pc [i n]]]
          (case i
            :nop (assoc program pc [:jmp n])
            :jmp (assoc program pc [:nop n])
            nil))
        program))

(defn solve [input]
  (let [program (->> input line-seq (map parse-instruction)
                     (map-indexed vector) (into {}))]
    {:part-1 (-> program run-til-end second)
     :part-2 (->> (variations program)
                  (map run-til-end)
                  (keep (fn [[result acc]] (when (= :term result) acc)))
                  first)}))
