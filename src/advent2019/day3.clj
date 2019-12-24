(ns advent2019.day3
  (:require [clojure.string :as str]))
 
(defn moves [path-str]
  (let [items (str/split path-str #",")]
    (map (fn [item]
           (let [[dir & length] item]
             (vector dir (Integer/parseInt (str/join "" length))))) items)))

(defn process-input [input]
  (map moves (map str/trim (str/split input #"\n"))))

(defn add-vec [v1 v2]
  (vec (map + v1 v2)))

(defn new-current [current-pos current-move]
  (let [[dir length] current-move]
    (add-vec current-pos (cond (= dir \R) [length 0]
                               (= dir \L) [(- length) 0]
                               (= dir \U) [0 length]
                               (= dir \D) [0 (- length)]))))

(defn turn-points [moves]
  (loop [current-pos [0 0]
         remaining moves
         turn-points []]
    (let [current-move (first remaining)]
      (if (nil? current-move)
        turn-points
        (let [new-pos (new-current current-pos current-move)]
          (recur new-pos (rest remaining) (conj turn-points [current-pos new-pos])))))))

(defn orientation [l]
  (let [[[x1 y1] [x2 y2]] l]
    (if (= x1 x2) :vertical :horizontal)))

(defn intersection [l1 l2]
  (let [o1 (orientation l1)
        o2 (orientation l2)]
    (if (= o1 o2)
      nil
      (let [[[l1x1 l1y1] [l1x2 l1y2]] (sort l1)
            [[l2x1 l2y1] [l2x2 l2y2]] (sort l2)]
        (cond
          (and
           (= o1 :vertical)
           (>= l1x1 l2x1)
           (<= l1x1 l2x2)
           (>= l2y1 l1y1)
           (<= l2y1 l1y2)) [l1x1 l2y1]
          (and
           (= o1 :horizontal)
           (>= l2x1 l1x1)
           (<= l2x1 l1x2)
           (>= l1y1 l2y1)
           (<= l1y1 l2y2)) [l2x1 l1y1]
          :else nil)))))

(defn fi [lines]
  (let [[l1 l2] lines]
    (for [x l1
          y l2
          :when (not (nil? (intersection x y)))]
      (intersection x y))))


(defn find-intersections [lines]
  (let [[l1 l2] lines]
    (loop [remaining l1
           intersections []]
      (let [current-line (first remaining)]
        (if (nil? current-line)
          intersections
          (for [x l1
                y l2
                :when (not (nil? (intersection x y)))]
            (intersection x y)))))))

(defn go [input]
  (let [iss (->>
             input
             process-input
             (map turn-points)
             fi)]
    (sort (map (fn [point]
                 (->>
                  point
                  (map #(Math/abs %))
                  (apply +))) iss))))
 
(defn day3 [input]
  (println "day 3 part 1: " (first (go input))))
