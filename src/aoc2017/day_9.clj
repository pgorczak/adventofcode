(ns aoc2017.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn char-seq [^java.io.Reader rdr]
   (let [chr (.read rdr)]
     (if (>= chr 0)
       (cons (char chr) (lazy-seq (char-seq rdr))))))

(defn process [s]
  (loop [[c & r] s
         state {:level 0 :garbage? false}
         acc {:score 0 :garbage 0}]
    (cond
      (nil? c) acc
      (= \! c) (recur (drop 1 r) state acc)
      (:garbage? state) (if (= c \>)
                            (recur r (assoc state :garbage? false) acc)
                            (recur r state (update acc :garbage inc)))
      (= \< c) (recur r (assoc state :garbage? true) acc)
      (= \{ c) (recur r (update state :level inc) acc)
      (= \} c) (recur r (update state :level dec)
                        (update acc :score + (:level state)))
      :else (recur r state acc))))

(defn solve []
  (let [input-stream (-> "2017-9" io/resource io/reader char-seq)
        {p1 :score p2 :garbage} (process input-stream)]
    {:part-1 p1
     :part-2 p2}))
