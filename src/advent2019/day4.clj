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

(defn count-num [[from to]]
  (->>
   (range from to)
   (filter non-decreasing?)
   (filter two-adj-digits-same?)))

(defn day4 [input]
  (println "day 4 part 1: " (count (count-num (process-input input)))))
