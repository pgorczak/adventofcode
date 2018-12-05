(ns aoc-utils)

(def contents-trim (comp clojure.string/trim slurp clojure.java.io/resource))
(def lines (comp line-seq clojure.java.io/reader clojure.java.io/resource))
