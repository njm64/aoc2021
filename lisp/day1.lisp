(defpackage :day1
  (:use :cl)
  (:export :run))

(in-package :day1)

(defun parse-input (lines)
  (mapcar #'parse-integer lines))

(defun count-part1 (lst)
  (loop for (a b) on lst
        while (and a b)
        count (> b a) into n
        finally (return n)))

(defun count-part2 (lst)
  (loop for (a b c d) on lst
        while (and a b c d)
        count (> (+ b c d) (+ a b c)) into n
        finally (return n)))

(defun run ()
  (let ((depths (parse-input (util:read-input "day1"))))
    (format t "Part 1: ~d~%" (count-part1 depths))
    (format t "Part 2: ~d~%" (count-part2 depths))))
