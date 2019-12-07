(ns advent2019.day2
  (:require [clojure.string :as str]))

(defn process-input [input]
  (let [input-vector (->>
                      (str/split input #",")
                      (map str/trim)
                      (map #(Integer/parseInt %))
                      vec)]
    (assoc (assoc input-vector 1 12) 2 2)))

(defn exec-opcode [instruction prog]
  (let [[opcode op1 op2 res] instruction
        op1-val (nth prog op1)
        op2-val (nth prog op2)
        operation (op-map opcode)]
    (assoc prog res (operation op1-val op2-val))))

(defn exec [prog]
  (loop [position 0
         current-prog prog]
    (println position)
    (let [instruction (subvec prog position (+ position 4))]
      (if (= (first instruction) 99)
        current-prog
        (recur (+ position 4) (exec-opcode instruction current-prog))))))

(defn day2 [input]
  (exec (process-input input)))
