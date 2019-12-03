(ns advent-of-code-2019.day3
  (:require
   [clojure.string :as str]))

"""
--- Day 3: Crossed Wires ---
The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........
Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........
These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
What is the Manhattan distance from the central port to the closest intersection?
"""

;; Setting up some vocabulary

(defn abs [n]
  (max n (- n)))

(defn hproj [interval]
  [(first (first interval)) (first (second interval))])

(defn horizontal? [interval]
  (let [projection (vproj interval)]
    (= (first projection) (second projection))))

(defn y-offset [horizontal-interval]
  (second (first horizontal-interval)))

(defn vproj [interval]
  [(second (first interval)) (second (second interval))])

(defn vertical? [interval]
  (let [projection (hproj interval)]
    (= (first projection) (second projection))))

(defn x-offset [vertical-interval]
  (first (first vertical-interval)))

;; Employing the vocabulary

(defn cross-intersection [hor ver]
  (let [point [(x-offset ver) (y-offset hor)]]
    (if (and
         (>= (first point) (apply min (hproj hor)))
         (<= (first point) (apply max (hproj hor)))
         (>= (second point) (apply min (vproj ver)))
         (<= (second point) (apply max (vproj ver))))
      point)))

(defn flat-intersection-aux [i1 i2]
  (let [i1* (sort i1)
        i2* (sort i2)
        start (max (first i1*) (first i2*))
        end (min (second i1*) (second i2*))]
    (if (<= start end)
      (cond
        (> start 0) start
        (< end 0) end
        :default 0))))

(defn flat-intersection [orientation s1 s2]
  (condp = orientation
    :horizontal
    (if (= (y-offset s1) (y-offset s2))
      (if-let [x (flat-intersection-aux (hproj s1) (hproj s2))]
        [x (y-offset s1)]))
    :vertical
    (if (= (x-offset s1) (x-offset s2))
      (if-let [y (flat-intersection-aux (vproj s1) (vproj s2))]
        [(x-offset s1) y]))))

(defn intersection [s1 s2]
  (cond
    (and (horizontal? s1) (vertical? s2))
    (cross-intersection s1 s2)
    (and (vertical? s1) (horizontal? s2))
    (cross-intersection s2 s1)
    (and (horizontal? s1) (horizontal? s2))
    (flat-intersection :horizontal s1 s2)
    (and (vertical? s1) (vertical? s2))
    (flat-intersection :vertical s1 s2)))

(defn intersection-distances [blue red]
  (for [b blue r red
        :let [i (intersection b r)]
        :when (some? i)]
    (+ (abs (first i)) (abs (second i)))))

(defn read-segment [prior seg]
  (let [origin (second prior)]
    (condp = (first seg)
      \L [origin [(- (first origin) (read-string (subs seg 1))) (second origin)]]
      \R [origin [(+ (first origin) (read-string (subs seg 1))) (second origin)]]
      \U [origin [(first origin) (+ (second origin) (read-string (subs seg 1)))]]
      \D [origin [(first origin) (- (second origin) (read-string (subs seg 1)))]])))

(defn read-wire [input]
  (->>
   (str/split input #",")
   (reductions read-segment [[0 0] [0 0]])))

(def wires
  (->> (str/split (slurp "src/advent_of_code_2019/day3.input") #"\r\n")
       (map read-wire)))

(intersection-distances (first wires) (second wires))
