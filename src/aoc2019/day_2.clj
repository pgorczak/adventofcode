(ns aoc2019.day-2
  (:require [aoc-utils]))

(defn load-state [text]
  ;; "5,6,7" -> {0 5, 1 6, 2 7}
  (->> (clojure.string/split text #",")
       (map #(Integer/parseInt %))
       (zipmap (range))))

;; The iterative style below exposes the evaluation history as a lazy
;; sequence. Alternatively, the evaluate function can be implemented with
;; loop/recur, integrating tick and not-halted? for 30% faster performance in
;; fewer lines of code. In turn, the iterative version lets you do things like
;; count steps, get intermediate outputs, or execute a specific number of steps
;; more easily.

;; program = state and program counter (pc)

(defn not-halted? [{:keys [state pc]}]
  (not= 99 (state pc)))

(defn tick [{:keys [state pc] :as program}]
  ;; get operands, destination and next pc
  (let [x   (state (state (+ pc 1)))
        y   (state (state (+ pc 2)))
        d   (state (+ pc 3))
        pc' (+ pc 4)]
    ;; update program based on operation
    (case (state pc)
      1  {:pc pc' :state (assoc state d (+ x y))}
      2  {:pc pc' :state (assoc state d (* x y))}
      99 program
      (throw (Exception. "invalid opcode")))))

(defn evaluate [init-state noun verb]
  (->> (iterate tick {:pc 0 :state (assoc init-state 1 noun 2 verb)})
       (drop-while not-halted?)
       first
       (#(get-in % [:state 0]))))

(defn find-input [init-state desired-output]
  (let [match? (fn [[n v]] (= desired-output (evaluate init-state n v)))
        candidates (for [noun (range 100) verb (range 100)] [noun verb])
        [noun verb] (first (filter match? candidates))]
    (-> (* 100 noun) (+ verb))))

(defn solve []
  (let [init-state (load-state (aoc-utils/contents-trim "2019-2"))]
    {:part-1 (evaluate init-state 12 2)
     :part-2 (find-input init-state 19690720)}))
