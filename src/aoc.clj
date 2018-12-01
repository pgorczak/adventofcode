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
      (println (solve year day)))
    (catch Exception e
      (println (->> [usage "" "" e] (string/join \newline)))
      (System/exit 1))))
