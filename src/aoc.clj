(ns aoc
  (:require [clojure.string :as string]))

(def usage
  (->> ["Advent of code entrypoint."
        ""
        "Usage: aoc YEAR DAY"]
       (string/join \newline)))

(defn ns-til-2019 [year day]
  (str "aoc" year ".day-" day))

(defn ns-from-2020 [year day]
  (str "aoc" year ".day-" (format "%02d" day)))

(defn make-ns [year day]
  (cond
    (< year 2020) (ns-til-2019 year day)
    :else (ns-from-2020 year day)))

(defn solve [year day]
  (let [ns (make-ns year day)
        _ (require (symbol ns))
        solve (resolve (symbol ns "solve"))]
    (solve)))

(defn -main [& args]
  (try
    (let [[year day] (map #(Integer/parseInt %) args)]
      (println (time (solve year day)))
      (shutdown-agents))
    (catch Exception e
      (println (str usage "\n\n"))
      (println e)
      (System/exit 1))))
