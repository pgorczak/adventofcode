(ns aoc2017.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines (-> "2017-8" io/resource io/reader line-seq))

(def ops {"inc" + "dec" - ">" > ">=" >= "==" = "<=" <= "<" < "!=" not=})

(defn parse-op [s]
  (let [[register op-str operand-str] (str/split s #" ")]
    [(get ops op-str) register (Integer/parseInt operand-str)]))

(defn make-apply [[o r x]]
  (fn [coll] (-> coll (get r 0) (o x))))

(defn make-update [[o r x]]
  (let [app (make-apply [o r x])]
    (fn [coll] (->> coll app (assoc coll r)))))

(defn line->instruction [l]
  (let [[update-str pred-str] (str/split l #" if ")
        update-op (-> update-str parse-op make-update)
        pred-op (-> pred-str parse-op make-apply)]
    (fn [coll] (if (pred-op coll) (update-op coll) coll))))

(def instrs (map line->instruction input-lines))

(defn solve []
  {:part-1 (->> (reduce #(%2 %1) {} instrs)
                vals
                (apply max))
   :part-2 (->> (reductions #(%2 %1) {} instrs)
                (keep vals)
                (apply concat)
                (apply max))})
