(ns aoc
  (:require [clojure.string :as string]))

(def usage
  (->> ["Advent of code entrypoint."
        ""
        "Usage: aoc YEAR DAY"]
       (string/join \newline)))

(defn solve [year day]
  (let [ns (str "aoc" year ".day-" day)
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
