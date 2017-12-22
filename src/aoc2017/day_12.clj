(ns aoc2017.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn group-map-by [f m]
  (->> (group-by f m)
       (map (fn [[k v]] [k (into {} v)]))
       (into {})))

(defn parse-line [s]
  (let [[from _ & to] (-> (str/replace s #"," "") (str/split #" "))]
    [(Integer/parseInt from) {:pipes (map #(Integer/parseInt %) to)}]))

(defn flood-fill [graph-map seed]
  (loop [[x & stack] [seed]
         gm graph-map]
    (cond
      (nil? x) gm
      (-> gm (get x) :visited some?) (recur stack gm)
      :else (recur
             (->> (get-in gm [x :pipes]) (into stack))
             (assoc-in gm [x :visited] true)))))

(defn cluster-groups [graph-map]
  (loop [graph-map graph-map
         groups []]
    (if (nil? graph-map)
      groups
      (let [fill (flood-fill graph-map (first (keys graph-map)))
            {group true remaining nil} (group-map-by #(:visited (val %)) fill)]
        (recur remaining (conj groups (->> group keys (into #{}))))))))

(defn solve []
  (let [input (->> "2017-12" io/resource io/reader line-seq
                   (map parse-line)
                   (into {}))]
    {:part-1 (->> (flood-fill input 0)
                  (filter #(contains? (val %) :visited))
                  count)
     :part-2 (->> (cluster-groups input) count)}))
