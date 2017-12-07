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

(defn make-message [{id :ids ws :weights acc :acc-weights}]
  (let [acc-freqs (frequencies acc)
        wrong-acc-weight (apply min-key acc-freqs acc)
        right-acc-weight (apply max-key acc-freqs acc)
        wrong-id (get (->> (map vector acc id) (into {})) wrong-acc-weight)
        wrong-weight (get (->> (map vector id ws) (into {})) wrong-id)
        right-weight (+ wrong-weight (- right-acc-weight wrong-acc-weight))]
    (str "weight of " wrong-id " should be " right-weight)))

(defn check-balance [{node-w :weight nested-cn :children}]
  (if (empty? nested-cn)
    node-w
    (let [c-balances (->> nested-cn vals (map check-balance))]
      (if (every? number? c-balances)
        (if (apply = c-balances)
          (apply + node-w c-balances)
          (make-message
           {:ids (keys nested-cn)
            :weights (->> nested-cn vals (map :weight))
            :acc-weights c-balances}))
        (->> c-balances (remove number?) first)))))

(defn solve []
  {:part-1 (root input-tree)
   :part-2 (-> input-tree nested check-balance)})
