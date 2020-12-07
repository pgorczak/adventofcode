(ns aoc2020.day-04)

(defn data->dict [d]
  (->> (map #(clojure.string/split % #":") d)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn parse-batch [b]
  (->> (clojure.string/split b #"\n\n")
       (map #(clojure.string/split % #"[\n\ ]"))
       (map data->dict)))

(defn number-in-range? [re low high s]
  (when-let [[_ n] (re-matches re s)]
    (<= low (Integer/parseInt n) high)))

(def rules
  {:byr #(number-in-range? #"([0-9]{4})" 1920 2002 %)
   :iyr #(number-in-range? #"([0-9]{4})" 2010 2020 %)
   :eyr #(number-in-range? #"([0-9]{4})" 2020 2030 %)
   :hgt #(or (number-in-range? #"([0-9]+)cm" 150 193 %)
             (number-in-range? #"([0-9]+)in" 59 76 %))
   :hcl #(re-matches #"#[a-f0-9]{6}" %)
   :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   :pid #(re-matches #"[0-9]{9}" %)})

(defn solve [input]
  (let [passports (-> input slurp parse-batch)
        all-present (filter #(->> rules keys (every? %)) passports)]
    {:part-1 (count all-present)
     :part-2 (->> all-present
                  (filter #(every? (fn [[k rule]] (-> k % rule)) rules))
                  count)}))
