(set-env!
 :source-paths #{"src"}
 :resource-paths #{"inputs"}
 :dependencies '[[org.clojure/clojure "1.8.0"]])

(deftask solve
  "Solve an advent of code puzzle."
  [y year VALUE int "The year."
   d day  VALUE int "The day."]
  (if (and (some? year) (some? day))
    (let [ns (str "aoc" year ".day-" day)
          _ (require (symbol ns))
          solve (resolve (symbol ns "solve"))]
      (fn [next-handler]
        (fn [fileset]
          (println (solve))
          (next-handler fileset))))
    (do (boot.util/fail "Year and day options are required!") (*usage*))))
