(ns aoc2019.day-5
  (:require [aoc-utils]))

;; from day 2
(defn load-state [text]
  ;; "5,6,7" -> {0 5, 1 6, 2 7}
  (->> (clojure.string/split text #",")
       (map #(Integer/parseInt %))
       (zipmap (range))))

;; from day 4 + expected list length
;; 123 5 -> (0 0 1 2 3)
(defn digits [x n]
  (loop [x x
         ds '()]
    (if (< (count ds) n)
      (recur (quot x 10) (conj ds (mod x 10)))
      ds)))

;; 102 -> 2
(defn get-op [inst]
  (mod inst 100))

;; 102 -> 0 0 1
(defn get-modes [inst n-params]
  (-> (quot inst 100)
      (digits n-params)
      (reverse)))

;; Number of parameters for each opcode
(def n-params
  {1 3
   2 3
   3 1
   4 1
   5 2
   6 2
   7 3
   8 3
   99 0})

;; Other than in day 2, pc is now part of the state-map. Input and output are
;; also new parts of it. Input is a single number and output is an append-only
;; vector.

(defn not-halted? [state]
  (not= 99 (state (:pc state))))

(defn tick [state]
  (let [pc (:pc state)
        inst (state pc)
        op (get-op inst)
        np (n-params op)
        ;; Get the raw parameter values
        params (->> (range (+ pc 1) (+ pc 1 np))
                    (map state))
        ;; Shortcut function for getting a raw parameter
        param #(nth params %)
        ;; Extract modes from the instruction code
        modes (get-modes inst np)
        ;; Shortcut function for loading a parameter based on its mode
        operand (fn [n]
                  (if (= 1 (nth modes n))
                    (param n)
                    (state (param n))))]
    (case op
      1 (-> (update state :pc + 4)
            (assoc (param 2) (+ (operand 0) (operand 1))))
      2 (-> (update state :pc + 4)
            (assoc (param 2) (* (operand 0) (operand 1))))
      3 (-> (update state :pc + 2)
            (assoc (param 0) (:input state)))
      4 (-> (update state :pc + 2)
            (update :output conj (operand 0)))
      5 (assoc state :pc (if-not (zero? (operand 0)) (operand 1) (+ pc 3)))
      6 (assoc state :pc (if (zero? (operand 0)) (operand 1) (+ pc 3)))
      7 (-> (update state :pc + 4)
            (assoc (param 2) (if (< (operand 0) (operand 1)) 1 0)))
      8 (-> (update state :pc + 4)
            (assoc (param 2) (if (= (operand 0) (operand 1)) 1 0)))
      99 state
      (throw (Exception. "invalid opcode")))))

(defn evaluate [init-state input]
  (->> (iterate tick (assoc init-state :pc 0 :input input :output []))
       (drop-while not-halted?)
       first
       :output
       last))

(defn solve []
  (let [init-state (load-state (aoc-utils/contents-trim "2019-5"))]
    {:part-1 (evaluate init-state 1)
     :part-2 (evaluate init-state 5)}))
