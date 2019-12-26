(ns advent2019.day5
  (:require [clojure.string :as str]))

(def op-map {1 [+ 4]
             2 [* 4]
             3 [:in 2]
             4 [:out 2]
             5 [:jit 3]
             6 [:jif 3]
             7 [:lt 4]
             8 [:eq 4]
             99 [:halt 0]})

(def modes {0 :position 1 :immediate})

(defn resolve-value [[value mode] prog]
  (cond (= mode :immediate) value
        (= mode :position) (nth prog value)))

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

(defn exec [prog input-value]
  (loop [position 0
         current-prog prog]
    (let [instruction (nth current-prog position)
          [op size] (get-op instruction)
          modes (parse-modes instruction)
          operands (take (dec size) (drop (inc position) current-prog))]
      (cond (= :out op) (do (println "out" (nth current-prog (first operands))) (recur (+ position size) current-prog))
            (= :in op) (recur (+ position size) (assoc current-prog (first operands) input-value))
            (= :halt op) (println "done")
            :else
            (let [operand-modes (partition 2 (interleave operands modes))
                  resolved-operands (map #(resolve-value % current-prog) operand-modes)]
              (cond (= :jit op) (if-not (zero? (first resolved-operands))
                                  (recur (second resolved-operands) current-prog)
                                  (recur (+ position size) current-prog))
                    (= :jif op) (if (zero? (first resolved-operands))
                                  (recur (second resolved-operands) current-prog)
                                  (recur (+ position size) current-prog))
                    (= :lt op) (let [first (first resolved-operands)
                                     second (second resolved-operands)
                                     lt-val (if (< first second) 1 0)]
                                 (recur (+ position size) (assoc current-prog (last operands) lt-val)))
                    (= :eq op) (let [first (first resolved-operands)
                                     second (second resolved-operands)
                                     eq-val (if (= first second) 1 0)]
                                 (recur (+ position size) (assoc current-prog (last operands) eq-val)))
                    :else
                    (let [new-prog (assoc current-prog (last operands) (apply op (butlast resolved-operands)))]
                      (recur (+ position size) new-prog))))))))


(defn process-input [input]
  (->>
   (str/split input #",")
   (map str/trim)
   (map #(Integer/parseInt %))
   vec))

(defn day5 [input]
  (println "day 5 part 1")
  (println (exec (process-input input) 1))
  
  (println "day 5 part 2")
  (println (exec (process-input input) 5)))
