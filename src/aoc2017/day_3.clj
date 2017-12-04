(ns aoc2017.day-3)

(defn p1-distance [x]
  (let [square-value (-> x (Math/sqrt) (Math/ceil)
                         ((fn [a] (if (zero? (mod a 2)) (inc a) a))))
        square-side (dec square-value)
        first-value (-> (dec square-side) (Math/pow 2) inc)
        offset (-> (- x first-value) (mod square-side) (+ (/ square-side -2) 1)
                   Math/abs (* -1) (+ (/ square-side 2)))]
    (int (- square-side offset))))


(defn p2-grid-memory [x]
  (let [step (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)])
        left-turn (fn [[x y]] [(- y) x])
        left-free? (fn [memory position direction]
                     (not (contains? memory (step position
                                                  (left-turn direction)))))
        neighbors (fn [position]
                    (let [straight (->> (iterate left-turn [1 0]) (take 4))
                          diagonal (->> (iterate left-turn [1 1]) (take 4))]
                      (map #(step position %) (concat straight diagonal))))
        current-value (fn [{m :memory p :position}] (get m p))
        next-state (fn [{m :memory p :position d :direction :as state}]
                     (let [new-d (if (left-free? m p d) (left-turn d) d)
                           new-p (step p new-d)
                           new-p-value (->> (neighbors new-p)
                                            (map #(get m % 0))
                                            (reduce +))]
                       (-> state
                           (assoc :position new-p :direction new-d)
                           (assoc-in [:memory new-p] new-p-value))))]

    (->> {:memory {[0 0] 1 [1 0] 1} :position [1 0] :direction [1 0]}
         (iterate next-state)
         (drop-while #(< (current-value %) x))
         (first)
         (current-value))))

{:part-1 (p1-distance 368078)
 :part-2 (p2-grid-memory 368078)}
