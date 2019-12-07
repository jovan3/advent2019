(ns advent2019.day1
  (:require [clojure.string :as str]))
            
(defn day1 [input]
  (println "day 1 part 1: " (->>
                             input
                             str/split-lines
                             (map #(Integer/parseInt %))
                             (map #(- (int (Math/floor (/ % 3))) 2))
                             (apply +))))
