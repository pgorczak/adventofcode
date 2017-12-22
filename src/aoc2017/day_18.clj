(ns aoc2017.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn duet-value [m x]
  (if (number? x) x (get m x 0)))

(defn duet-update [m k f x]
  (assoc m k (f (duet-value m k) (duet-value m x))))

(def dispatch
  {:set (fn [reg x frame]
          (assoc frame reg (duet-value frame x)))
   :add (fn [reg x frame]
          (duet-update frame reg + x))
   :mul (fn [reg x frame]
          (duet-update frame reg * x))
   :mod (fn [reg x frame]
          (duet-update frame reg mod x))
   :jgz (fn [x y frame]
          (if (pos? (duet-value frame x))
              (update frame :pc + (dec (duet-value frame y)))
              frame))})

(defn dynamic [inst]
  (fn [x y frame]
    ((inst frame) frame x y)))

(defn execute [instrs {pc :pc :as frame}]
  (if (< -1 pc (count instrs))
    (-> ((get instrs pc) frame)
        (update :pc inc))
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

(defn parse-line [dispatch l]
  (let [[i x y] (str/split (str/trim l) #" ")]
    (-> (get dispatch (keyword i) (dynamic (keyword i)))
        (partial (parse-value x) (parse-value y)))))

(def p1-init-frame
  {:pc 0
   :snd (fn [f r _]
          (assoc f :snd/value (duet-value f r)))
   :rcv (fn [f r _]
          (if (zero? (duet-value f r)) f (assoc f :rcv/value (:snd/value f))))})

(def p2-init-frame
  {:pc 0 :sent 0 :inbox [] :outbox nil
   :snd (fn [f r _]
          (-> (assoc f :outbox (duet-value f r)) (update :sent inc)))
   :rcv (fn [f r _]
          (if-let [in (first (:inbox f))]
            (-> (assoc f r in) (update :inbox #(subvec % 1)))
            (update f :pc dec)))})

(defn part-2 [program]
  (loop [f0 (assoc p2-init-frame "p" 0 :id 0)
         f1 (assoc p2-init-frame "p" 1 :id 1)]
    (let [[f0+ f1+] (-> (map (partial execute program) [f0 f1]) exchange)]
      (if (every? true? (map stalled? [f0 f1] [f0+ f1+]))
          (:sent f1+)
          (recur f0+ f1+)))))

(defn solve []
  (let [input (->> "2017-18" io/resource io/reader line-seq)
        program (->> input (map (partial parse-line dispatch)) (into []))]
    {:part-1 (->> (iterate (partial execute program) p1-init-frame)
                  (drop-while #(nil? (:rcv/value %)))
                  first :rcv/value)
     :part-2 (part-2 program)}))
