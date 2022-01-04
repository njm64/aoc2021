(defpackage :day1
  (:use :cl)
  (:export :run))

(in-package :day1)

(defun parse-input (lines)
  (mapcar #'parse-integer lines))

(defun part1 (lst)
  (loop for (a b) on lst
        while (and a b)
        count (> b a) into n
        finally (return n)))

(defun part2 (lst)
  (loop for (a b c d) on lst
        while (and a b c d)
        count (> (+ b c d) (+ a b c)) into n
        finally (return n)))
