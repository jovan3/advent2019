(ns advent2019.day5
  (:require [clojure.string :as str]))

(def op-map {1 [+ 4]
             2 [* 4]
             3 [:in 2]
             4 [:out 2]
             99 [:halt nil]})

(defn resolve-value [[value mode] prog]
  (cond (= mode :immediate) value
        (= mode :position) (nth prog value)))

(def modes {0 :position 1 :immediate})

(defn parse-modes [opcode]
  (let [padded-opcode (format "%05d" opcode)
        opcode-digits (str padded-opcode)]
    (->>
     opcode-digits
     (map str)
     (map #(Integer/parseInt %))
     (take 3)
     reverse
     (map modes))))

(defn get-op [opcode]
  (if (= opcode 99) (op-map opcode) 
      (op-map (->>
               opcode
               str
               last
               str
               Integer/parseInt))))


(defn exec-opcode2 [prog position]
  (let [instruction (nth prog position)
        [op size] (get-op instruction)
        modes (parse-modes instruction)
        operands (take (dec size) (drop (inc position) prog))]
    (cond (= :out op) (do (println "out" (nth prog (first operands))) prog)
          (= :in op) (assoc prog (first operands) 1)
          :else
          (let [operand-modes (partition 2 (interleave operands modes))
                resolved-operands (map #(resolve-value % prog) operand-modes)]
            (assoc prog (last operands) (apply op (butlast resolved-operands)))))))

(defn exec [prog]
  (loop [position 0
         current-prog prog]
    (let [[op size] (get-op (nth current-prog position))]
      (if (= :halt op)
        (println "done")
        (recur (+ position size) (exec-opcode2 current-prog position))))))

(defn process-input [input]
  (->>
   (str/split input #",")
   (map str/trim)
   (map #(Integer/parseInt %))
   vec))

(defn day5 [input]
  (println (exec (process-input input))))
