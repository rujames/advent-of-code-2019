(ns advent-of-code-2019.day17
  (:require [clojure.string :as str]))

"
--- Day 17: Set and Forget ---
An early warning system detects an incoming solar flare and automatically activates the ship's electromagnetic shield. Unfortunately, this has cut off the Wi-Fi for many small robots that, unaware of the impending danger, are now trapped on exterior scaffolding on the unsafe side of the shield. To rescue them, you'll have to act quickly!

The only tools at your disposal are some wired cameras and a small vacuum robot currently asleep at its charging station. The video quality is poor, but the vacuum robot has a needlessly bright LED that makes it easy to spot no matter where it is.

An Intcode program, the Aft Scaffolding Control and Information Interface (ASCII, your puzzle input), provides access to the cameras and the vacuum robot. Currently, because the vacuum robot is asleep, you can only access the cameras.

Running the ASCII program on your Intcode computer will provide the current view of the scaffolds. This is output, purely coincidentally, as ASCII code: 35 means #, 46 means ., 10 starts a new line of output below the current one, and so on. (Within a line, characters are drawn left-to-right.)

In the camera output, # represents a scaffold and . represents open space. The vacuum robot is visible as ^, v, <, or > depending on whether it is facing up, down, left, or right respectively. When drawn like this, the vacuum robot is always on a scaffold; if the vacuum robot ever walks off of a scaffold and begins tumbling through space uncontrollably, it will instead be visible as X.

In general, the scaffold forms a path, but it sometimes loops back onto itself. For example, suppose you can see the following view from the cameras:

..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^..
Here, the vacuum robot, ^ is facing up and sitting at one end of the scaffold near the bottom-right of the image. The scaffold continues up, loops across itself several times, and ends at the top-left of the image.

The first step is to calibrate the cameras by getting the alignment parameters of some well-defined points. Locate all scaffold intersections; for each, its alignment parameter is the distance between its left edge and the left edge of the view multiplied by the distance between its top edge and the top edge of the view. Here, the intersections from the above image are marked O:

..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^..
For these intersections:

The top-left intersection is 2 units from the left of the image and 2 units from the top of the image, so its alignment parameter is 2 * 2 = 4.
The bottom-left intersection is 2 units from the left and 4 units from the top, so its alignment parameter is 2 * 4 = 8.
The bottom-middle intersection is 6 from the left and 4 from the top, so its alignment parameter is 24.
The bottom-right intersection's alignment parameter is 40.
To calibrate the cameras, you need the sum of the alignment parameters. In the above example, this is 76.

Run your ASCII program. What is the sum of the alignment parameters for the scaffold intersections?
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
       99 (assoc state :terminated? true))))

(defn compile-map [program-sequence]
  (apply (partial assoc {}) (interleave (range) program-sequence)))

(def program (->> (str/split (slurp "src/advent_of_code_2019/day17.input") #",")
     (map read-string)
     (compile-map)))

(defn initial-state [program]
  {:memory program
   :pointer 0
   :out []
   :relative-base 0})

(defn run [program]
  (some #(if (:terminated? %) (:out %)) (iterate step (initial-state program))))

(def current-view (apply str (map char (run program))))

;; (println current-view)
;; ..............#####............................
;; ..............#...#............................
;; ..............#...#............................
;; ..............#...#............................
;; ..............#...#............................
;; ..............#...#............................
;; ..............#...#.....#############..........
;; ..............#...#.....#...........#..........
;; ..............#.#####...#.#############........
;; ..............#.#.#.#...#.#.........#.#........
;; ..............#.#.#############.....#.#........
;; ..............#.#...#...#.#...#.....#.#........
;; ..............#######...#.#.###########........
;; ................#.......#.#.#.#.....#..........
;; ........#########.......#.#####.....#####......
;; ........#...............#...#...........#......
;; ........#...............#...#...........#......
;; ........#...............#...#...........#......
;; #########.....###########...#...........#......
;; #.............#.............#...........#......
;; #.............#.........#######.........#......
;; #.............#.........#...#.#.........#......
;; #.............#############.#.#.........######^
;; #.......................#.#.#.#................
;; #.......................#####.#................
;; #.........................#...#................
;; #.........................#...#................
;; #.........................#...#................
;; #.........................#...#................
;; #.........................#...#................
;; #######...................#...#................
;; ......#...................#...#................
;; ......#...................#####................
;; ......#........................................
;; ......#........................................
;; ......#........................................
;; ......#........................................
;; ......#........................................
;; ......#####....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#....................................
;; ..........#############........................

(defn is-intersection? [grid x y]
  (every? identity (for [[dx dy] [[-1 0] [0 0] [1 0] [0 -1] [0 1]]] (= (nth (nth grid (+ y dy)) (+ x dx)) \#))))

(defn part-one []
  (apply + (let [grid (str/split current-view #"\n")]
             (for [x (range 1 46)
                   y (range 1 46)]
               (if (is-intersection? grid x y) (* x y) 0)))))

(part-one)


