(defpackage :day7
  (:use :cl) 
  (:export :parse-input :part1 :part2))

(in-package :day7)

(defun parse-input (lines)
  (mapcar #'parse-integer (str:split #\, (first lines))))

(defun make-fuel-table (max)
  "Build a table with the cost for travelling distance i at each index"
  (loop for i from 0 to max
        for f = 0 then (+ f i)
        collect f into r
        finally (return (coerce r 'vector))))

(defun calc-fuel (crabs cost-fn pos)
  "Calculate the fuel to move all crabs to a given position"
  (loop for c in crabs sum (funcall cost-fn (abs (- pos c)))))

(defun calc-min-fuel (crabs f)
  "Calculate minimum fuel using a cost function f"
  (loop for i to (apply #'max crabs) minimize (calc-fuel crabs f i)))

(defun part1 (crabs)
  "Part 1: Cost is just the identity function"
  (calc-min-fuel crabs #'identity))

(defun part2 (crabs)
  "Part 2: Build a lookup table with fuel costs"
  (let ((fuel-table (make-fuel-table (apply #'max crabs))))
    (calc-min-fuel crabs #'(lambda (n) (aref fuel-table n)))))

