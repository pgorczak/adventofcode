(ns aoc2017.day-6)

(def input (-> (clojure.java.io/resource "2017-6") slurp))

(def banks (->> (clojure.string/split input #"\s") (map #(Integer/parseInt %))))

(defn redistribute [banks]
  (let [banks (vec banks)
        max-p (dec (count banks))
        move (fn [p] (if (< p max-p) (inc p) 0))
        amount (apply max banks)
        init-p (.indexOf banks amount)]
    (loop [banks (assoc banks init-p 0)
           p (move init-p)
           n amount]
      (if (zero? n)
        banks
        (recur (update banks p inc) (move p) (dec n))))))

(defn until-repeat [s]
  (loop [[x & r] s
         seen #{}]
    (if (contains? seen x)
      {:seen seen :element x :rest r}
      (recur r (conj seen x)))))

(defn count-until-repeat [s]
  (let [{seen :seen} (until-repeat s)]
    (count seen)))

(defn count-infinite-loop [s]
  (let [{element :element rest :rest} (until-repeat s)
        {seen :seen} (until-repeat (cons element rest))]
    (count seen)))

(defn solve []
  (let [redistributions (->> banks (iterate redistribute))]
    {:part-1 (count-until-repeat redistributions)
     :part-2 (count-infinite-loop redistributions)}))
