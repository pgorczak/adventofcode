(ns aoc2018.day-10
  (:require [aoc-utils]))

(defn parse-entry [s]
  (->> s
       ((juxt #(subs % 10 16) #(subs % 18 24) #(subs % 36 38) #(subs % 40 42)))
       (map clojure.string/trim)
       (map #(Integer/parseInt %))
       (zipmap [:x :y :vx :vy])))

(defn step-entry [{:keys [vx vy] :as e}]
  (-> e (update :x + vx) (update :y + vy)))

(defn extents [ps]
  (let [xs (map :x ps)
        ys (map :y ps)]
    {:x-min (apply min xs) :x-max (apply max xs)
     :y-min (apply min ys) :y-max (apply max ys)}))

(defn area [ps]
  (let [{:keys [x-min x-max y-min y-max]} (extents ps)]
    (* (- x-max x-min) (- y-max y-min))))

(defn first-min-key [k xs]
  (loop [[x1 x2 & xs] (map (fn [x] [(k x) x]) xs)]
    (if (< (first x1) (first x2))
      (second x1)
      (recur (conj xs x2)))))

(defn sky->str [s]
  (let [{:keys [x-min x-max y-min y-max]} (extents s)
        stars (->> s (map (juxt :x :y)) set)]
    (->> (for [y (range y-min (inc y-max))]
           (for [x (range x-min (inc x-max))]
             (if (contains? stars [x y]) \# \space)))
         (map #(apply str %))
         (clojure.string/join \newline))))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-10") (map parse-entry))
        [secs sky] (->> (iterate #(mapv step-entry %) input)
                        (map-indexed vector)
                        (first-min-key (comp area second)))]
    {:part-1 (str \newline (sky->str sky) \newline)
     :part-2 secs}))
