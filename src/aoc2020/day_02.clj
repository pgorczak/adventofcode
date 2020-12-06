(ns aoc2020.day-02)

(defn parse-entry [l]
  (let [[_ lo hi ch pw] (re-find #"(\d+)-(\d+)\ ([a-z]):\ ([a-z]+)" l)]
    {:lo (Integer/parseInt lo) :hi (Integer/parseInt hi) :c (first ch) :pw pw}))

(defn valid-1? [{:keys [lo hi c pw]}]
  (<= lo (count (filter #(= c %) pw)) hi))

(defn valid-2? [{:keys [lo hi c pw]}]
  (let [pos1 (= c (nth pw (dec lo)))
        pos2 (= c (nth pw (dec hi)))]
    (and (or pos1 pos2) (not (and pos1 pos2)))))

(defn solve [input]
  (let [entries (->> input line-seq (map parse-entry))]
    {:part-1 (->> entries (filter valid-1?) count)
     :part-2 (->> entries (filter valid-2?) count)}))
