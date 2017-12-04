(ns aoc2017.day-4)

(def input (slurp (clojure.java.io/resource "2017-4")))

(defn p1-valid?
  "All words must be unique."
  [words]
  (= (count words) (count (set words))))

(defn p2-valid?
  "All normalized words must be unique."
  [words]
  (let [nws (map sort words)]
    (= (count nws) (count (set nws)))))

(defn valid-phrases [pred list]
  (->> list
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (filter pred)
       (count)))

{:part-1 (valid-phrases p1-valid? input)
 :part-2 (valid-phrases p2-valid? input)}
