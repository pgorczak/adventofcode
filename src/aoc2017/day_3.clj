(ns aoc2017.day-3)

(defn distance
  [x]
  (let [square-value (-> x (Math/sqrt) (Math/ceil)
                         ((fn [a] (if (zero? (mod a 2)) (inc a) a))))
        square-side (dec square-value)
        first-value (-> (dec square-side) (Math/pow 2) inc)
        offset (-> (- x first-value) (mod square-side) (+ (/ square-side -2) 1)
                   Math/abs (* -1) (+ (/ square-side 2)))]
    (int (- square-side offset))))

(defn step
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn left-turn
  [[x y]]
  [(- y) x])

(defn grid-step
  [{m :memory p :position d :direction :as state}]
  (let [d-left (left-turn d)
        p-left (step p d-left)]
    (if (contains? m p-left)
      (assoc state :position (step p d))
      (assoc state :position p-left :direction d-left))))

(def neighborhood [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]])

(defn grid-allocate
  [{m :memory p :position d :direction :as state}]
  (->> (map #(step p %) neighborhood)
       (map #(get m % 0))
       (reduce +)
       (assoc-in state [:memory p])))

(defn grid-value
  [{m :memory p :position}]
  (get m p))

(defn next-grid-value
  [low]
  (->> {:memory {[0 0] 1 [1 0] 1} :position [1 0] :direction [1 0]}
       (iterate (comp grid-allocate grid-step))
       (drop-while #(< (grid-value %) low))
       (first)
       (grid-value)))

{:part-1 (distance 368078)
 :part-2 (next-grid-value 368078)}
