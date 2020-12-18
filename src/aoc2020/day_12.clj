(ns aoc2020.day-12)

(def dir->dxy {0 [1 0] 1 [0 1] 2 [-1 0] 3 [0 -1]})
(def char->dxy {:E [1 0] :N [0 1] :W [-1 0] :S [0 -1]})

(defn read-instruction [s]
  (let [[_ action value] (re-matches #"([NSEWLRF])([0-9]+)" s)]
    [(keyword action) (Integer/parseInt value)]))

(defn turn [dir degs]
  (let [x (/ degs 90)]
    (mod (+ dir x) (count dir->dxy))))

(defn +xy [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn move [pos delta times]
  (-> (iterate #(+xy delta %) pos)
      (nth times)))

(defn rot90 [[x y] sign]
  [(* -1 sign y) (* sign x)])

(defn rotate [pos sign degs]
  (-> (iterate #(rot90 % sign) pos)
      (nth (/ degs 90))))

(defn action-1 [state [act value]]
  (case act
    (:E :N :W :S) (update state :pos move (char->dxy act) value)
    :L (update state :dir turn value)
    :R (update state :dir turn (- value))
    :F (update state :pos move (-> state :dir dir->dxy) value)))

(defn action-2 [state [act value]]
  (case act
    (:E :N :W :S) (update state :wp move (char->dxy act) value)
    :L (update state :wp rotate 1 value)
    :R (update state :wp rotate -1 value)
    :F (update state :pos move (:wp state) value)))

(defn solve [input]
  (let [ins (map read-instruction (line-seq input))]
    {:part-1 (let [{[x y] :pos} (reduce action-1 {:pos [0 0] :dir 0} ins)]
               (+ (Math/abs x) (Math/abs y)))
     :part-2 (let [{[x y] :pos} (reduce action-2 {:pos [0 0] :wp [10 1]} ins)]
               (+ (Math/abs x) (Math/abs y)))}))
