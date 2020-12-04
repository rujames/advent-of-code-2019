(ns advent-of-code-2019.day15
  (:require [clojure.string :as str]))

"
--- Day 15: Oxygen System ---
Out here in deep space, many things can go wrong. Fortunately, many of those things have indicator lights. Unfortunately, one of those lights is lit: the oxygen system for part of the ship has failed!

According to the readouts, the oxygen system must have failed days ago after a rupture in oxygen tank two; that section of the ship was automatically sealed once oxygen levels went dangerously low. A single remotely-operated repair droid is your only option for fixing the oxygen system.

The Elves' care package included an Intcode program (your puzzle input) that you can use to remotely control the repair droid. By running that program, you can direct the repair droid to the oxygen system and fix the problem.

The remote control program executes the following steps in a loop forever:

Accept a movement command via an input instruction.
Send the movement command to the repair droid.
Wait for the repair droid to finish the movement operation.
Report on the status of the repair droid via an output instruction.
Only four movement commands are understood: north (1), south (2), west (3), and east (4). Any other command is invalid. The movements differ in direction, but not in distance: in a long enough east-west hallway, a series of commands like 4,4,4,4,3,3,3,3 would leave the repair droid back where it started.

The repair droid can reply with any of the following status codes:

0: The repair droid hit a wall. Its position has not changed.
1: The repair droid has moved one step in the requested direction.
2: The repair droid has moved one step in the requested direction; its new position is the location of the oxygen system.
You don't know anything about the area around the repair droid, but you can figure it out by watching the status codes.

For example, we can draw the area using D for the droid, # for walls, . for locations the droid can traverse, and empty space for unexplored locations. Then, the initial state looks like this:

      
      
   D  
      
      
To make the droid go north, send it 1. If it replies with 0, you know that location is a wall and that the droid didn't move:

      
   #  
   D  
      
      
To move east, send 4; a reply of 1 means the movement was successful:

      
   #  
   .D 
      
      
Then, perhaps attempts to move north (1), south (2), and east (4) are all met with replies of 0:

      
   ## 
   .D#
    # 
      
Now, you know the repair droid is in a dead end. Backtrack with 3 (which you already know will get a reply of 1 because you already know that location is open):

      
   ## 
   D.#
    # 
      
Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south again (2) gets a reply of 0, and then west (3) gets a reply of 2:

      
   ## 
  #..#
  D.# 
   #  
Now, because of the reply of 2, you know you've found the oxygen system! In this example, it was only 2 moves away from the repair droid's starting position.

What is the fewest number of movement commands required to move the repair droid from its starting position to the location of the oxygen system?
"


(defn add [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (+ x y))
         :pointer (+ (:pointer state) 4)))
     
(defn multiply [state x y output]
  (assoc state
         :memory (assoc (:memory state) output (* x y))
         :pointer (+ (:pointer state) 4)))

(defn input [state address]
  (assoc state
         :memory (assoc (:memory state) address (:direction state))
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
       99 0)))

(defn move [[x y] dir]
  (condp = dir
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn success [state]
  (condp = (last (:out state))
    2 (-> state
          (update :depth inc)
          (update :position move (:direction state))
          (assoc :out [])
          (assoc :found true))
    1 (-> state
          (update :depth inc)
          (update :position move (:direction state))
          (assoc :out []))
    0 nil))

(defn attempt-move [state direction]
  (loop [state (assoc state :direction direction)]
    (let [next-state (step state)]
      (if (seq (:out next-state))
        (success next-state)
        (recur next-state)))))

(defn bfs [state]
  (loop [queue [state]
         visited #{}]
    (let [state (first queue)
          successful-results
          (->> [1 2 3 4]
               (keep #(attempt-move state %))
               (filter #(not (visited (:position %)))))]
      (if-let [success-state (some #(if (:found %) %) successful-results)]
        (:depth success-state)
        (recur (apply conj (vec (rest queue)) successful-results) (conj visited (:position state)))))))

(defn compile-map [program-sequence]
  (apply (partial assoc {}) (interleave (range) program-sequence)))

(def program (->> (str/split (slurp "src/advent_of_code_2019/day15.input") #",")
     (map read-string)
     (compile-map)))

(def initial-state
  {:memory program
   :pointer 0
   :direction 1
   :position [0 0]
   :depth 0
   :out []
   :relative-base 0})

(attempt-move initial-state 1)

;; (bfs initial-state)
;; => 216

"
--- Part Two ---
You quickly repair the oxygen system; oxygen gradually fills the area.

Oxygen starts in the location containing the repaired oxygen system. It takes one minute for oxygen to spread to all open locations that are adjacent to a location that already contains oxygen. Diagonal locations are not adjacent.

In the example above, suppose you've used the droid to explore the area fully and have the following map (where locations that currently contain oxygen are marked O):

 ##   
#..## 
#.#..#
#.O.# 
 ###  
Initially, the only location which contains oxygen is the location of the repaired oxygen system. However, after one minute, the oxygen spreads to all open (.) locations that are adjacent to a location containing oxygen:

 ##   
#..## 
#.#..#
#OOO# 
 ###  
After a total of two minutes, the map looks like this:

 ##   
#..## 
#O#O.#
#OOO# 
 ###  
After a total of three minutes:

 ##   
#O.## 
#O#OO#
#OOO# 
 ###  
And finally, the whole region is full of oxygen after a total of four minutes:

 ##   
#OO## 
#O#OO#
#OOO# 
 ###  
So, in this example, all locations contain oxygen after 4 minutes.

Use the repair droid to get a complete map of the area. How many minutes will it take to fill with oxygen?
"

(defn find-oxygen-system [state]
  (loop [queue [state]
         visited #{}]
    (let [state (first queue)
          successful-results
          (->> [1 2 3 4]
               (keep #(attempt-move state %))
               (filter #(not (visited (:position %)))))]
      (if-let [success-state (some #(if (:found %) %) successful-results)]
        success-state
        (recur (apply conj (vec (rest queue)) successful-results) (conj visited (:position state)))))))

(def second-state (assoc (find-oxygen-system initial-state) :depth 0))

(defn flood-fill [state]
  (loop [queue [state]
         visited #{}]
    (let [state (first queue)
          successful-results
          (->> [1 2 3 4]
               (keep #(attempt-move state %))
               (filter #(not (visited (:position %)))))]
      (if (and (empty? successful-results) (empty? (rest queue)))
        (:depth state)
        (recur (apply conj (vec (rest queue)) successful-results) (conj visited (:position state)))))))

;; (flood-fill second-state)
;; => 326
