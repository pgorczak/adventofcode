(ns aoc2017.day-4)

(def input (slurp (clojure.java.io/resource "2017-4")))

(defn p1-valid?
  "All words must be unique."
  [words]
  (= (count words) (count (set words))))

(defn char-counts
  "foo -> {f: 1 o: 2}"
  [w]
  (let [count-char (fn [acc c] (->> (get acc c 0) inc (assoc acc c)))]
    (reduce count-char {} w)))

(defn p2-valid?
  "All char-counts must be unique."
  [words]
  (let [ccs (map char-counts words)]
    (= (count ccs) (count (set ccs)))))

(defn valid-phrases [pred list]
  (->> list
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (filter pred)
       (count)))

{:part-1 (valid-phrases p1-valid? input)
 :part-2 (valid-phrases p2-valid? input)}
