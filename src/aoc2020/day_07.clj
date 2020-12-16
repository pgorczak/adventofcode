(ns aoc2020.day-07)

(defn parse-content [s]
  (when-let [[_ n bag] (re-find #"(\d+) (\w+ \w+) bag" s)]
    [bag (Integer/parseInt n)]))

(defn parse-rule [s]
  (let [[_ bag contents] (re-find #"^(\w+ \w+) bags contain ([\w, ]+)" s)]
    [bag (->> (clojure.string/split contents #", ") (map parse-content)
              (into {}))]))

(defn outer-bags [rules inner-bag]
  (loop [outers '()
         [inner & inners] [inner-bag]]
    (if-not inner
      (set outers)
      (let [containers (keep (fn [[bag conts]] (when (conts inner) bag)) rules)]
        (recur (into outers containers) (into inners containers))))))

(defn total-bag-count [rules outer-bag]
  ;; sorry, not tail recursive
  (if-let [contents (seq (rules outer-bag))]
    (->> contents
         (map (fn [[bag n]] (* n (total-bag-count rules bag))))
         (apply + 1))
    1))

(defn solve [input]
  (let [rules (->> input line-seq (map parse-rule) (into {}))]
    {:part-1 (->> (outer-bags rules "shiny gold") count)
     :part-2 (->> (total-bag-count rules "shiny gold") dec)}))
