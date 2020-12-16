(ns aoc2020.day-09)

(defn is-sum? [xs n]
  (loop [[x & xs] (filter #(< % n) xs)]
    (when x
      (or ((set xs) (- n x)) (recur xs)))))

(defn find-sum [xs n]
  (loop [[x & xs] xs
         sum 0
         summands []]
    (when-not (and x (> sum n))
      (if (= sum n)
        summands
        (recur xs (+ sum x) (conj summands x))))))

(defn solve [input]
  (let [xmas (->> input line-seq (map #(Long/parseLong %)))
        invalid (->> (iterate rest xmas)
                     (map (fn [s] [(take 25 s) (nth s 25)]))
                     (keep (fn [[xs n]] (when-not (is-sum? xs n) n)))
                     first)]
    {:part-1 invalid
     :part-2 (->> (iterate rest xmas)
                  (keep #(find-sum % invalid))
                  first
                  (#(+ (apply min %) (apply max %))))}))
