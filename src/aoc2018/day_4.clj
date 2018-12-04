(ns aoc2018.day-4
  (:require [aoc-utils]))

(def date-fmt (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm"))

(defn parse-log [l]
  (let [[_ date msg] (re-find #"^\[(.+)\]\s(.*)$" l)
        record {:date (.parse date-fmt date)}]
    (case msg
      "wakes up" (assoc record :type :wake)
      "falls asleep" (assoc record :type :sleep)
      (assoc record
             :type :begin
             :guard (->> msg (re-find #"#(\d+)") second (Integer/parseInt))))))

(defn minutes-between [t1 t2]
  (let [t1-hr (.getHours t1)
        t2-min (.getMinutes t2)]
    (if (zero? t1-hr)
      (range (.getMinutes t1) t2-min)
      (range 0 t2-min))))

(defn sleep-times [log]
  (loop [[{t :type d :date :as e} & log] log
         guard nil
         sleep nil
         stats {}]
    (if e
      (case t
        :wake (if sleep
                (recur log guard nil
                       (update stats guard
                               (partial concat (minutes-between sleep d))))
                (recur log guard sleep stats))
        :sleep (if sleep
                 (recur log guard sleep stats)
                 (recur log guard d stats))
        :begin (recur log (:guard e) nil stats))
      (zipmap (keys stats) (->> (vals stats) (map frequencies))))))

(defn solve []
  (let [input (->> (aoc-utils/lines "2018-4") (map parse-log) (sort-by :date)
                   sleep-times)]
    {:part-1 (let [guard (->> input
                              (apply max-key #(->> % val vals (apply +)))
                              key)
                   minute (->> (get input guard) (apply max-key val) key)]
               (* guard minute))
     :part-2 (->> input
                  (map (fn [[k v]] [k (apply max-key val v)]))
                  (apply max-key (comp second second))
                  ((fn [[g [m _]]] (* m g))))}))
