(ns aoc2017.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines (-> "2017-7" io/resource io/reader line-seq))

(defn parse-line [l]
  (let [normalized (str/replace l #"[\(\),]" "")
        [id weight _ & children] (str/split normalized #" ")]
    [id {:weight (Integer/parseInt weight)
         :children (if (some? children) (set children) #{})}]))

(def input-tree (->> (map parse-line input-lines) (into {})))

(defn root [tree]
  (->> tree vals
       (map :children)
       (apply concat)
       (into #{})
       (clojure.set/difference (set (keys tree)))
       first))

(defn nested
  ([tree] (nested tree (root tree)))
  ([tree node]
   (->> (get tree node)
        :children
        (map (fn [c] [c (nested tree c)]))
        (into {})
        (assoc (get tree node) :children))))

(defn minority-majority [c]
  (let [fs (frequencies c)]
    [(apply min-key fs c) (apply max-key fs c)]))

(defn at-index-of [v c a]
  (nth a (-> c vec (.indexOf v))))

(defn make-message [nested-cn acc-weights]
  (let [[bad-acc good-acc] (minority-majority acc-weights)
        id (at-index-of bad-acc acc-weights (keys nested-cn))
        bad-weight (get-in nested-cn [id :weight])
        good-weight (+ bad-weight (- good-acc bad-acc))]
    (str "weight of " id " should be " good-weight)))

(defn check-balance [{node-w :weight nested-cn :children}]
  (if (empty? nested-cn)
    node-w
    (let [c-balances (->> nested-cn vals (map check-balance))]
      (if (apply = c-balances)
        (apply + node-w c-balances)
        (-> (make-message nested-cn c-balances) (Exception.) throw)))))

(defn solve []
  {:part-1 (root input-tree)
   :part-2 (try
            (-> input-tree nested check-balance)
            (catch Exception e (.getMessage e)))})
