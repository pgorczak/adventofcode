(ns aoc2018.day-8
  (:require [aoc-utils :refer [contents-trim]]))

(defn make-tree [data]
  (let [[n-child n-meta & data] data
        [children data] (-> (iterate (fn [[c d]]
                                        (let [[t d] (make-tree d)]
                                          [(conj c t) d]))
                                     [[] data])
                            (nth n-child))]
    [{:children children :meta (take n-meta data)}
     (drop n-meta data)]))

(defn sum-meta [{:keys [meta children]}]
  (->> (map sum-meta children)
       (into meta)
       (reduce +)))

(defn node-value [{:keys [meta children]}]
  (if (empty? children)
    (apply + meta)
    (->> meta
         (keep #(nth children (dec %) nil))
         (map node-value)
         (apply +))))

(defn solve []
  (let [input (->> (clojure.string/split (contents-trim "2018-8") #" ")
                   (map #(Integer/parseInt %))
                   make-tree
                   first)]
    {:part-1 (sum-meta input)
     :part-2 (node-value input)}))
