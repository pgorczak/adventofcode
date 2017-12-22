(ns aoc2017.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn duet-value [m x]
  (if (number? x) x (get m x 0)))

(defn duet-update [m k f x]
  (assoc m k (f (duet-value m k) (duet-value m x))))

(def basic-frame
  {:pc 0
   :fn/set (fn [f x y] (assoc f x (duet-value f y)))
   :fn/add (fn [f x y] (duet-update f x + y))
   :fn/mul (fn [f x y] (duet-update f x * y))
   :fn/mod (fn [f x y] (duet-update f x mod y))
   :fn/jgz (fn [f x y]
             (if (pos? (duet-value f x))
                 (update f :pc + (dec (duet-value f y)))
                 f))})

(defn execute [program {pc :pc :as frame}]
  (if-let [[inst args] (get program pc nil)]
    (-> (frame inst) (apply frame args) (update :pc inc))
    frame))

(defn communicate [from-frame to-frame]
  (if-let [x (:outbox from-frame)]
    [(assoc from-frame :outbox nil) (update to-frame :inbox #(conj % x))]
    [from-frame to-frame]))

(defn exchange [[f1 f2]]
  (let [[f1 f2] (communicate f1 f2)
        [f2 f1] (communicate f2 f1)]
    [f1 f2]))

(defn stalled? [frame next-frame]
  (= (:pc frame) (:pc next-frame)))

(defn parse-value [v]
  (cond
    (nil? v) nil
    (re-matches #"[a-z]" v) v
    :else (Integer/parseInt v)))

(defn parse-line [l]
  (let [[i arg1 arg2] (str/split (str/trim l) #" ")]
    [(keyword (str "fn/" i)) (map parse-value [arg1 arg2])]))

(def p1-init-frame
  (assoc
   basic-frame
   :fn/snd (fn [f r _]
             (assoc f :snd (duet-value f r)))
   :fn/rcv (fn [f r _]
             (if (zero? (duet-value f r)) f (assoc f :rcv (f :snd))))))

(def p2-init-frame
  (assoc
   basic-frame :sent 0 :inbox [] :outbox nil
   :fn/snd (fn [f r _]
             (-> (assoc f :outbox (duet-value f r)) (update :sent inc)))
   :fn/rcv (fn [f r _]
             (if-let [in (first (:inbox f))]
               (-> (assoc f r in) (update :inbox #(subvec % 1)))
               (update f :pc dec)))))

(defn part-2 [program]
  (loop [f0 (assoc p2-init-frame "p" 0)
         f1 (assoc p2-init-frame "p" 1)]
    (let [[f0+ f1+] (-> (map (partial execute program) [f0 f1]) exchange)]
      (if (every? true? (map stalled? [f0 f1] [f0+ f1+]))
          (:sent f1+)
          (recur f0+ f1+)))))

(defn solve []
  (let [input (->> "2017-18" io/resource io/reader line-seq)
        program (->> input (map parse-line) (into []))]
    {:part-1 (->> (iterate (partial execute program) p1-init-frame)
                  (drop-while #(nil? (:rcv %)))
                  first :rcv)
     :part-2 (part-2 program)}))
