(ns advent-of-code-2019.day11
  (:require [clojure.string :as str]))

"""
--- Day 11: Space Police ---
On the way to Jupiter, you're pulled over by the Space Police.

'Attention, unmarked spacecraft! You are in violation of Space Law! All spacecraft must have a clearly visible registration identifier! You have 24 hours to comply or be sent to Space Jail!'

Not wanting to be sent to Space Jail, you radio back to the Elves on Earth for help. Although it takes almost three hours for their reply signal to reach you, they send instructions for how to power up the emergency hull painting robot and even provide a small Intcode program (your puzzle input) that will cause it to paint your ship appropriately.

There's just one problem: you don't have an emergency hull painting robot.

You'll need to build a new emergency hull painting robot. The robot needs to be able to move around on the grid of square panels on the side of your ship, detect the color of its current panel, and paint its current panel black or white. (All of the panels are currently black.)

The Intcode program will serve as the brain of the robot. The program uses input instructions to access the robot's camera: provide 0 if the robot is over a black panel or 1 if the robot is over a white panel. Then, the program will output two values:

First, it will output a value indicating the color to paint the panel the robot is over: 0 means to paint the panel black, and 1 means to paint the panel white.
Second, it will output a value indicating the direction the robot should turn: 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees.
After the robot turns, it should always move forward exactly one panel. The robot starts facing up.

The robot will continue running for a while like this and halt when it is finished drawing. Do not restart the Intcode computer inside the robot during this process.

For example, suppose the robot is about to start running. Drawing black panels as ., white panels as #, and the robot pointing the direction it is facing (< ^ > v), the initial state and region near the robot looks like this:

.....
.....
..^..
.....
.....
The panel under the robot (not visible here because a ^ is shown instead) is also black, and so any input instructions at this point should be provided 0. Suppose the robot eventually outputs 1 (paint white) and then 0 (turn left). After taking these actions and moving forward one panel, the region now looks like this:

.....
.....
.<#..
.....
.....
Input instructions should still be provided 0. Next, the robot might output 0 (paint black) and then 0 (turn left):

.....
.....
..#..
.v...
.....
After more outputs (1,0, 1,0):

.....
.....
..^..
.##..
.....
The robot is now back where it started, but because it is now on a white panel, input instructions should be provided 1. After several more outputs (0,1, 1,0, 1,0), the area looks like this:

.....
..<#.
...#.
.##..
.....
Before you deploy the robot, you should probably have an estimate of the area it will cover: specifically, you need to know the number of panels it paints at least once, regardless of color. In the example above, the robot painted 6 panels at least once. (It painted its starting panel twice, but that panel is still only counted once; it also never painted the panel it ended on.)

Build a new emergency hull painting robot and run the Intcode program on it. How many panels does it paint at least once?
"""

(defn paint [state colour]
  (assoc state
         :colours (assoc (:colours state) (:current state) colour)
         :painted? true))

(defn plus-minus [turn-code]
  (condp = turn-code
    0 dec
    1 inc))

(defn next-direction [state turn]
  (mod ((plus-minus turn) (:direction state)) 4))

(defn move [state turn]
  (let [current (:current state)
        direction (next-direction state turn)]
    (assoc state
         :direction direction
         :current (condp = direction
                      0 [(first current) (inc (second current))]
                      1 [(inc (first current)) (second current)]
                      2 [(first current) (dec (second current))]
                      3 [(dec (first current)) (second current)])
         :painted? false)))

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
         :memory (assoc (:memory state) output (get (:colours state) (:current state) 0))
         :pointer (+ (:pointer state) 2)))

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
       99 (assoc state :terminated? true))))

(defn run [program initial-colour]
  (loop [state {:memory program
                :pointer 0
                :out []
                :relative-base 0
                :current [0 0]
                :colours {[0 0] initial-colour}
                :direction 0
                :terminated? false
                :painted? false}]
    (let [next-state (step state)]
      (if (:terminated? next-state)
        next-state
        (if (not= (:out next-state) (:out state))
          (recur (if (:painted? next-state)
                   (move next-state (last (:out next-state)))
                   (paint next-state (last (:out next-state)))))
          (recur next-state))))))

(defn compile-map [program-sequence]
  (apply (partial assoc {}) (interleave (range) program-sequence)))

(def program (->> (str/split (slurp "src/advent_of_code_2019/day11.input") #",")
     (map read-string)
     (compile-map)))

;; (count (:colours (run program 0)))
;; => 1883

"""
--- Part Two ---
You're not sure what it's trying to paint, but it's definitely not a registration identifier. The Space Police are getting impatient.

Checking your external ship cameras again, you notice a white panel marked 'emergency hull painting robot starting panel'. The rest of the panels are still black, but it looks like the robot was expecting to start on a white panel, not a black one.

Based on the Space Law Space Brochure that the Space Police attached to one of your windows, a valid registration identifier is always eight capital letters. After starting the robot on a single white panel instead, what registration identifier does it paint on your hull?
"""

(defn line [colours line-number from to]
  (println line-number from to)
  (->> (range from (inc to))
       (map #(if (= (get colours [% line-number] 0) 1) "#" " "))
       (str/join)))

(defn display [colours]
  (let [left (apply min (map first (keys colours)))
        right (apply max (map first (keys colours)))
        top (apply max (map second (keys colours)))
        bottom (apply min (map second (keys colours)))]
    (println (->> (range top (dec bottom) -1)
         (map #(line colours % left right))
         (str/join "\r\n")))))

;; (display (:colours (run program 1)))
;; 
;;  ##  ###  #  #  ##  #  # ###  #### #  #   
;; #  # #  # #  # #  # #  # #  # #    #  #   
;; #  # #  # #  # #    #  # #  # ###  ####   
;; #### ###  #  # # ## #  # ###  #    #  #   
;; #  # #    #  # #  # #  # # #  #    #  #   
;; #  # #     ##   ###  ##  #  # #    #  #
;;
;; => nil

