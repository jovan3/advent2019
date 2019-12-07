(ns advent2019.day1
  (:require [clojure.string :as str]))

(defn calculate-fuel [weight]
  (- (int (Math/floor (/ weight 3))) 2))

(defn progression [weight]
  (->>
   (iterate calculate-fuel weight)
   (take-while (partial < 0))
   rest ;; can be avoided by using the intermediate calculation for part 1 as func. input
   (apply +)))

(defn process-input [input]
  (->>
   (str/split-lines input)
   (map #(Integer/parseInt %))))

(defn day1 [input]
  (let [processed-input (process-input input)]
    (println "day 1 part 1: " (apply + (map calculate-fuel processed-input)))
    (println "day 1 part 2: " (apply + (map progression processed-input)))))
