(ns advent2019.day6
  (:require [clojure.string :as str]
            [loom.graph :as g]
            [loom.alg :as alg]))

(defn process-input [input]
  (map #(str/split % #"\)" ) (str/split-lines input)))

(defn day6 [input]
  (let [g (apply g/digraph (process-input input))]
    (println "day 6 part 1:" (count (flatten (mapcat vals (vals (alg/all-pairs-shortest-paths g))))))))
