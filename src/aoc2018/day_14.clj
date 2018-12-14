(ns aoc2018.day-14
  (:require [aoc-utils]))

(defn digits [x]
  (let [tens (quot x 10)]
    (if (zero? tens) [x] [tens (- x (* 10 tens))])))

(defn evolve [{:keys [recipes elves] :as state}]
  (let [rs' (->> elves (map #(nth recipes %)) (apply +) digits (into recipes))
        n (count rs')
        es' (->> elves (mapv (fn [x] (-> (+ x 1 (nth recipes x)) (mod n)))))]
    (assoc state :recipes rs' :elves es')))

(defn find-sequence [recipes input]
  (let [input (str input)
        matches? #(= input (apply str %))
        n (count input)
        recipes (drop-while #(< (count %) n) recipes)]
    (loop [[r & recipes] recipes
           i 0]
      (if-let [j (first (filter (fn [j] (matches? (subvec r j (+ j n))))
                                (range i (- (count r) n))))]
        j
        (recur recipes (- (count r) n))))))

(defn solve []
  (let [input (Integer/parseInt (aoc-utils/contents-trim "2018-14"))
        n-recipes (+ input 10)
        recipes (->> (iterate evolve {:recipes [3 7] :elves [0 1]})
                     (map :recipes))]
    {:part-1 (->> recipes (drop-while #(< (count %) n-recipes)) first
                  (#(apply str (subvec % input))))
     :part-2 (find-sequence recipes input)}))
