(ns advent-of-code-2019.day13
  (:require [clojure.string :as str]))

"""
--- Day 13: Care Package ---
As you ponder the solitude of space and the ever-increasing three-hour roundtrip for messages between you and Earth, you notice that the Space Mail Indicator Light is blinking. To help keep you sane, the Elves have sent you a care package.

It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all the way on the other end of the ship. Surely, it won't be hard to build your own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent (your puzzle input). It has a primitive screen capable of drawing square tiles on a grid. The software draws tiles to the screen with output instructions: every three output instructions specify the x position (distance from the left), y position (distance from the top), and tile id. The tile id is interpreted as follows:

0 is an empty tile. No game object appears in this tile.
1 is a wall tile. Walls are indestructible barriers.
2 is a block tile. Blocks can be broken by the ball.
3 is a horizontal paddle tile. The paddle is indestructible.
4 is a ball tile. The ball moves diagonally and bounces off objects.
For example, a sequence of output values like 1,2,3,6,5,4 would draw a horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game exits?
"""

(defn add [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (+ x y))
         :pointer (+ (:pointer state) 4)))
     
(defn multiply [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (* x y))
         :pointer (+ (:pointer state) 4)))

(defn input [state output]) ;; Undefined?

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

(defn run [program]
  (loop [state {:memory program
                :pointer 0
                :out []
                :relative-base 0}]
    (let [next-state (step state)]
      (if (int? next-state)
        (:out state)
        (recur next-state)))))

(defn compile-map [program-sequence]
  (apply (partial assoc {}) (interleave (range) program-sequence)))

(def program (->> (str/split (slurp "src/advent_of_code_2019/day13.input") #",")
     (map read-string)
     (compile-map)))

(defn draw-tile [screen [x y id]]
  (assoc screen [x y] id))

"""
(->> (run program)
     (partition 3)
     (reduce draw-tile {})
     vals
     frequencies)
=> {0 504, 2 369, 1 87, 3 1, 4 1}
"""

"""
--- Part Two ---
The game didn't run because you didn't put in any quarters. Unfortunately, you did not bring any quarters. Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.

The arcade cabinet has a joystick that can move left and right. The software reads the position of the joystick with input instructions:

If the joystick is in the neutral position, provide 0.
If the joystick is tilted to the left, provide -1.
If the joystick is tilted to the right, provide 1.
The arcade cabinet also has a segment display capable of showing a single number that represents the player's current score. When three output instructions specify X=-1, Y=0, the third output instruction is not a tile; the value instead specifies the new score to show in the segment display. For example, a sequence of output values like -1,0,12345 would show 12345 as the player's current score.

Beat the game by breaking all the blocks. What is your score after the last block is broken?
"""

(defn input [state to]
  (assoc state
         :memory (assoc (:memory state) to (:joystick state))
         :pointer (+ (:pointer state) 2)))

(defn consume-output [state]
  (if (= (count (:out state)) 3)
    (-> (condp = (nth (:out state) 2)
          3 (assoc state :paddle (vec (take 2 (:out state))))
          4 (assoc state :ball (vec (take 2 (:out state))))
          state)
        (assoc :screen (draw-tile (:screen state) (:out state))
               :out []))
    state))

(defn adjust-joystick [state]
  (assoc state
         :joystick (Integer/signum (- (first (:ball state)) (first (:paddle state))))))

(defn adjust [state]
  (->> state
       consume-output
       adjust-joystick))

(defn run [program]
  (loop [state {:memory program
                :pointer 0
                :joystick 0
                :ball [0 0]
                :paddle [0 0]
                :out []
                :relative-base 0
                :screen {}}]
    (let [next-state (step state)]
      (if (int? next-state)
        (:screen state)
        (recur (adjust next-state))))))

(def free-play
  (assoc program 0 2))

;; (get (run free-play) [-1 0])
;; => 19210
