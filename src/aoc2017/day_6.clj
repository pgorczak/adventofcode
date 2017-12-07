(ns aoc2017.day-6)

(def input (-> (clojure.java.io/resource "2017-6") slurp))

(def banks (->> (clojure.string/split input #"\s") (map #(Integer/parseInt %))))

(defn redistribute [banks]
  (let [n-blocks (apply max banks)
        bank-i (.indexOf banks n-blocks)
        banks-init (assoc banks bank-i 0)]
    (->> banks count range cycle ; repeating indexes
         (drop (inc bank-i)) ; starting from max-i + 1
         (take n-blocks) ; indexes for all incs
         frequencies ; index: n-incs for redistribution
         (merge-with + banks-init))))

(defn until-repeat [s]
  (loop [[x & r] s
         seen #{}]
    (if (contains? seen x)
      {:seen seen :rest (cons x r)}
      (recur r (conj seen x)))))

(defn solve []
  (let [redistributions (->> banks vec (iterate redistribute))]
    {:part-1 (-> redistributions until-repeat :seen count)
     :part-2 (-> redistributions until-repeat :rest until-repeat :seen count)}))
