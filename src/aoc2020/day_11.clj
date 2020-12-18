(ns aoc2020.day-11)

(defn read-row [y row]
  (map-indexed (fn [x tile] [[x y] tile]) row))

(defn read-tiles [rows]
  (->> (map-indexed read-row rows)
       (apply concat)
       (into {})))

(def directions (-> (set (for [x [-1 0 1] y [-1 0 1]] [x y])) (disj [0 0])))

(defn +xy [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn direct-neighbors [xy]
  (map #(+xy xy %) directions))

(defn direction-neighbor [tiles xy dxy]
  (->> (iterate #(+xy dxy %) xy)
       (drop 1)
       (take-while tiles)
       (filter #(not= \. (tiles %)))
       first))

(defn direction-neighbors [tiles xy]
  (map #(direction-neighbor tiles xy %) directions))

(defn update-seat [seats n [xy s] neighbors]
  (let [occ (-> (map seats neighbors) frequencies (get \# 0))]
    (cond
      (and (= s \L) (= occ 0)) [xy \#]
      (and (= s \#) (>= occ n)) [xy \L]
      :else nil)))

(defn stable-occupied [seats n neighbors]
  (loop [seats seats]
    (if-let [updates (->> (map #(update-seat seats n %1 %2) seats neighbors)
                          (filter some?)
                          seq)]
      (recur (into seats updates))
      (-> seats vals frequencies (get \# 0)))))

(defn solve [input]
  (let [tiles (read-tiles (line-seq input))
        seats (into {} (filter #(not= \. (val %))) tiles)
        neighbors-1 (map #(filter seats (direct-neighbors %)) (keys seats))
        neighbors-2 (map #(direction-neighbors tiles %) (keys seats))]
    {:part-1 (stable-occupied seats 4 neighbors-1)
     :part-2 (stable-occupied seats 5 neighbors-2)}))
