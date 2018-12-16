(ns aoc2018.day-16
  (:require [clojure.set]
            [aoc-utils]))

(defn fn->op [f]
  (fn [regs a b c] (assoc regs c (f regs a b))))

(defn make-rr [f]
  (fn->op (fn [regs a b] (f (get regs a) (get regs b)))))

(defn make-ri [f]
  (fn->op (fn [regs a b] (f (get regs a) b))))

(defn make-ir [f]
  (fn->op (fn [regs a b] (f a (get regs b)))))

(defn gt [a b]
  (if (> a b) 1 0))

(defn eq [a b]
  (if (= a b) 1 0))

(def ops
  {:addr (make-rr +)
   :addi (make-ri +)
   :mulr (make-rr *)
   :muli (make-ri *)
   :banr (make-rr bit-and)
   :bani (make-ri bit-and)
   :borr (make-rr bit-or)
   :bori (make-ri bit-or)
   :setr (fn->op (fn [regs a _] (get regs a)))
   :seti (fn->op (fn [regs a _] a))
   :gtir (make-ir gt)
   :gtri (make-ri gt)
   :gtrr (make-rr gt)
   :eqir (make-ir eq)
   :eqri (make-ri eq)
   :eqrr (make-rr eq)})

(defn behaves-like? [op regs regs' a b c]
  (= regs' (op regs a b c)))

(defn read-sample [ls]
  {:before (-> (first ls) (subs 8) read-string)
   :instr (read-string (str "[" (second ls) "]"))
   :after (-> (last ls) (subs 8) read-string)})

(defn possible-ops [{[_ a b c] :instr :keys [before after]}]
  (->> (filter #(behaves-like? (val %) before after a b c) ops)
       (map key)
       set))

(defn map-vals [f m]
  (reduce #(update %1 %2 f) m (keys m)))

(defn infer-opcodes [samples]
  (loop [candidates
         (->> samples
              (map (fn [{[o _ _ _] :instr :as s}] {o (possible-ops s)}))
              (reduce (partial merge-with clojure.set/intersection)))
         codes
         {}]
    (if (empty? candidates)
      codes
      (let [[code ops] (apply min-key (comp count val) candidates)
            _ (assert (= 1 (count ops)))
            op (first ops)]
        (recur (->> (dissoc candidates code)
                    (map-vals #(disj % op)))
               (assoc codes code op))))))

(defn apply-op [codes regs [o a b c]]
  ((get ops (get codes o)) regs a b c))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-16") (partition-by empty?))
        samples (->> input (take-nth 2) butlast (map read-sample))
        program (map #(read-string (str "[" % "]")) (last input))]
    {:part-1 (->> samples
                  (map (comp count possible-ops))
                  (filter #(>= % 3))
                  count)
     :part-2 (->> program
                  (reduce (partial apply-op (infer-opcodes samples)) [0 0 0 0])
                  first)}))
