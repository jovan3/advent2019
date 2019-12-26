(ns advent2019.core
  (:require [advent2019.day1 :as day1]
            [advent2019.day2 :as day2]
            [advent2019.day3 :as day3]
            [advent2019.day4 :as day4]
            [advent2019.day5 :as day5]
            [advent2019.day6 :as day6]
            [clojure.java.io :as io]))

(defn resource-to-string [resource_filename]
  (slurp (io/file (io/resource resource_filename))))

(defn -main
  [& args]
  (comment (println (day1/day1 (resource-to-string "day1_input")))
           (println (day2/day2 (resource-to-string "day2_input")))
           (println (day3/day3 (resource-to-string "day3_input")))
           (println (day4/day4 (resource-to-string "day4_input")))
           (println (day5/day5 (resource-to-string "day5_input"))))
  (println (day6/day6 (resource-to-string "day6_input"))))
