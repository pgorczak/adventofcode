(ns aoc2020.day-03)

(defn count-trees [lines right down]
  (let [width (count (first lines))]
    (->> (map #(mod (* right %) width) (range))
         (map nth (take-nth down lines))
         (filter #(= \# %))
         count)))

(defn solve [input]
  (let [lines (line-seq input)]
    {:part-1 (count-trees lines 3 1)
     :part-2 (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
                  (map #(apply count-trees lines %))
                  (apply *))}))
