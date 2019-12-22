(ns advent-of-code-2019.day9
  (:require [clojure.string :as str]))

"""
--- Day 9: Sensor Boost ---
You've just said goodbye to the rebooted rover and left Mars when you receive a faint distress signal coming from the asteroid belt. It must be the Ceres monitoring station!

In order to lock on to the signal, you'll need to boost your sensors. The Elves send up the latest BOOST program - Basic Operation Of System Test.

While BOOST (your puzzle input) is capable of boosting your sensors, for tenuous safety reasons, it refuses to do so until the computer it runs on passes some checks to demonstrate it is a complete Intcode computer.

Your existing Intcode computer is missing one key feature: it needs support for parameters in relative mode.

Parameters in mode 2, relative mode, behave very similarly to parameters in position mode: the parameter is interpreted as a position. Like position mode, parameters in relative mode can be read from or written to.

The important difference is that relative mode parameters don't count from address 0. Instead, they count from a value called the relative base. The relative base starts at 0.

The address a relative mode parameter refers to is itself plus the current relative base. When the relative base is 0, relative mode parameters and position mode parameters with the same value refer to the same address.

For example, given a relative base of 50, a relative mode parameter of -7 refers to memory address 50 + -7 = 43.

The relative base is modified with the relative base offset instruction:

Opcode 9 adjusts the relative base by the value of its only parameter. The relative base increases (or decreases, if the value is negative) by the value of the parameter.
For example, if the relative base is 2000, then after the instruction 109,19, the relative base would be 2019. If the next instruction were 204,-34, then the value at address 1985 would be output.

Your Intcode computer will also need a few other capabilities:

The computer's available memory should be much larger than the initial program. Memory beyond the initial program starts with the value 0 and can be read or written like any other memory. (It is invalid to try to access memory at a negative address, though.)
The computer should have support for large numbers. Some instructions near the beginning of the BOOST program will verify this capability.
Here are some example programs that use these features:

109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input and produces a copy of itself as output.
1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
104,1125899906842624,99 should output the large number in the middle.
The BOOST program will ask for a single input; run it in test mode by providing it the value 1. It will perform a series of checks on each opcode, output any opcodes (and the associated parameter modes) that seem to be functioning incorrectly, and finally output a BOOST keycode.

Once your Intcode computer is fully functional, the BOOST program should report no malfunctioning opcodes when run in test mode; it should only output a single value, the BOOST keycode. What BOOST keycode does it produce?
"""

(defn add [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (+ x y))
         :pointer (+ (:pointer state) 4)))
     
(defn multiply [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (* x y))
         :pointer (+ (:pointer state) 4)))

(defn input [state output]
  (assoc state
         :memory (assoc (:memory state) output (first (:in state)))
         :pointer (+ (:pointer state) 2)
         :in (rest (:in state))))

(defn output [state x]
  (assoc state
         :pointer (+ (:pointer state) 2)
         :out (conj (:out state) x)))

(defn jump-if-true [state p x]
  (if (> p 0)
    (assoc state
           :pointer x)
    (assoc state
           :pointer (+ (:pointer state) 3))))

(defn jump-if-false [state p x]
  (if (= p 0)
      (assoc state
             :pointer x)
      (assoc state
             :pointer (+ (:pointer state) 3))))

(defn less-than [state x y output]
    (if (< x y)
      (assoc state
             :memory (assoc (:memory state) output 1)
             :pointer (+ (:pointer state) 4))
      (assoc state
             :memory (assoc (:memory state) output 0)
             :pointer (+ (:pointer state) 4))))

(defn equals [state x y output]
  (if (= x y)
      (assoc state
             :memory (assoc (:memory state) output 1)
             :pointer (+ (:pointer state) 4))
      (assoc state
             :memory (assoc (:memory state) output 0)
             :pointer (+ (:pointer state) 4))))

(defn adjust-base [state x]
  (assoc state
         :pointer (+ (:pointer state) 2)
         :relative-base (+ (:relative-base state) x)))

(defn get-modes [instruction]
  (->> [(quot instruction 100)
        (quot instruction 1000)
        (quot instruction 10000)]
       (map #(mod % 10))
       (map #(condp = %
               0 :position
               1 :immediate
               2 :relative))))

(defn param [parameter mode state]
  (condp = mode
    :position (get (:memory state) parameter 0)
    :immediate parameter
    :relative (get (:memory state) (+ (:relative-base state) parameter) 0)))

(defn takep [state modes n]
  (let [arg (get (:memory state) (+ (:pointer state) (inc n)))]
    (condp = (nth modes n)
      :position (get (:memory state) arg 0)
      :immediate arg
      :relative (get (:memory state) (+ (:relative-base state) arg) 0))))

(defn putp [state modes n]
  (let [arg (get (:memory state) (+ (:pointer state) (inc n)))]
    (condp = (nth modes n)
      :position arg
      :relative (+ (:relative-base state) arg))))
  
(defn step [state]
  (let [instruction (get (:memory state) (:pointer state) 0)
        modes (get-modes instruction)]
    (condp = (mod instruction 100)
       1 (add state (takep state modes 0) (takep state modes 1) (putp state modes 2))
       2 (multiply state (takep state modes 0) (takep state modes 1) (putp state modes 2))
       3 (input state (putp state modes 0))
       4 (output state (takep state modes 0))
       5 (jump-if-true state (takep state modes 0) (takep state modes 1))
       6 (jump-if-false state (takep state modes 0) (takep state modes 1))
       7 (less-than state (takep state modes 0) (takep state modes 1) (putp state modes 2))
       8 (equals state (takep state modes 0) (takep state modes 1) (putp state modes 2))
       9 (adjust-base state (takep state modes 0))
       99 0)))

(defn run [program inputs]
  (loop [state {:memory program
                :pointer 0
                :in inputs
                :out []
                :relative-base 0}]
    (let [next-state (step state)]
      (if (int? next-state)
        (:out state)
        (recur next-state)))))

(defn compile-map [program-sequence]
  (apply (partial assoc {}) (interleave (range) program-sequence)))

(def prog (->> (str/split (slurp "src/advent_of_code_2019/day9.input") #",")
     (map read-string)
     (compile-map)))

;; (run prog [1])

"""
--- Part Two ---
You now have a complete Intcode computer.

Finally, you can lock on to the Ceres distress signal! You just need to boost your sensors using the BOOST program.

The program runs in sensor boost mode by providing the input instruction the value 2. Once run, it will boost the sensors automatically, but it might take a few seconds to complete the operation on slower hardware. In sensor boost mode, the program will output a single value: the coordinates of the distress signal.

Run the BOOST program in sensor boost mode. What are the coordinates of the distress signal?
"""

;; (run prog [2])
