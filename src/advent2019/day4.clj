(ns advent2019.day4
  (:require [clojure.string :as str]))

(defn process-input [input]
  (map #(Integer/parseInt %) (str/split input #"-")))

(defn non-decreasing? [number]
  (let [num (str number)]
    (->>
     (str/split num #"")
     (map #(Integer/parseInt %))
     sort
     (str/join "")
     (= num))))

(defn two-adj-digits-same? [number]
  (let [num (str number)
        adj-pairs (partition 2 1 num)]
    (some (fn [[x y]] (= x y)) adj-pairs)))

(defn part2 [number]
  (some (fn [x] (= 2 (.length (str/join "" x))))
        (let [num (str number)]
          (partition-by identity num))))

(defn find-matches [[from to] adj-predicate]
  (->>
   (range from to)
   (filter non-decreasing?)
   (filter adj-predicate)))

(defn day4 [input]
  (println "day 4 part 1: " (count (find-matches (process-input input) two-adj-digits-same?)))
  (println "day 4 part 2: " (count (find-matches (process-input input) part2))))

