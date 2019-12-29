(ns advent2019.day7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combi])
  (:use [advent2019.day5 :as com]))

(defn calculate-signal [prog input-sequence]
  (last (reduce (fn [out x] (conj out (com/exec prog [x (last out)]))) [0] input-sequence)))

(defn find-max [prog]
  (let [all-sequences (combi/permutations (range 0 5))]
    (apply max (map #(calculate-signal prog %) all-sequences))))

(defn day7 [input]
  (println "day 7 part 1:" (find-max (com/process-input input))))
