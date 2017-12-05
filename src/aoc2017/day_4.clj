(ns aoc2017.day-4)

(def input (slurp (clojure.java.io/resource "2017-4")))

(defn distinct-words? [words]
  (apply distinct? words))

(defn distinct-anagrams? [words]
  (->> (map sort words) (apply distinct?)))

(defn valid-phrases [pred list]
  (->> list
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (filter pred)
       (count)))

(defn solve []
  {:part-1 (valid-phrases distinct-words? input)
   :part-2 (valid-phrases distinct-anagrams? input)})
