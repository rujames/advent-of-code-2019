(ns advent-of-code-2019.day4
  (:require [clojure.string :as str]))

"""
--- Day 4: Secure Container ---
You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

It is a six-digit number.
The value is within the range given in your puzzle input.
Two adjacent digits are the same (like 22 in 122345).
Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
Other than the range rule, the following are true:

111111 meets these criteria (double 11, never decreases).
223450 does not meet these criteria (decreasing pair of digits 50).
123789 does not meet these criteria (no double).
How many different passwords within the range given in your puzzle input meet these criteria?
"""

;; Cheating here since the input has no trailing zeros
(defn digit-seq [n]
  (map (comp read-string str) (str n)))

(defn non-decreasing-digits? [n]
  (not= (reduce (fn [highest d] (if (>= d highest) d 10)) (digit-seq n)) 10))

;; If the digits of n are non-decreasing, repeated digits must be adjacent
(defn repeated-digit? [n]
  (< (count (set (digit-seq n))) 6))

(def input
  (->>
   (str/split (slurp "src/advent_of_code_2019/day4.input") #"-")
   (map read-string)))

(def input-range
  (range (first input) (+ 1 (second input))))

(->> input-range
     (filter non-decreasing-digits?)
     (filter repeated-digit?)
     (count))

"""
--- Part Two ---
An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.

Given this additional criterion, but still ignoring the range rule, the following are now true:

112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
How many different passwords within the range given in your puzzle input meet all of the criteria?
"""

(defn strict-digit-pair? [n]
  (some? (some (partial = 2) (vals (frequencies (digit-seq n))))))

(->> input-range
     (filter non-decreasing-digits?)
     (filter repeated-digit?)
     (filter strict-digit-pair?)
     (count))
