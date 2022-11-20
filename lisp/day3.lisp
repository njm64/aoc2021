(defpackage :day3
  (:use :cl :trivia)
  (:export :parse-input :part1 :part2))

(in-package :day3)

(defun num-bits (lst)
  (length (first lst)))

(defun count-bits (lst i)
  (loop for s in lst
        counting (eql (elt s i) #\0) into c0
        counting (eql (elt s i) #\1) into c1
        finally (return (values c0 c1))))

(defun most-common-bit (lst i)
  (multiple-value-bind (c0 c1) (count-bits lst i)
    (if (> c0 c1) #\0 #\1)))

(defun least-common-bit (lst i)
  (multiple-value-bind (c0 c1) (count-bits lst i)
    (if (> c0 c1) #\1 #\0)))

(defun filter-by-bit (lst i b)
  (remove-if-not (lambda (n) (eql (elt n i) b)) lst))

(defun bin->int (bits)
  (parse-integer (concatenate 'string bits) :radix 2))

(defun calc-gamma (lst)
  (loop for i from 0 below (num-bits lst)
        collect (most-common-bit lst i) :into bits
        finally (return (bin->int bits))))

(defun calc-epsilon (lst)
  (loop for i from 0 below (num-bits lst)
        collect (least-common-bit lst i) :into bits
        finally (return (bin->int bits))))

(defun calc-oxygen (lst)
  (loop for i from 0 below (num-bits lst)
        do (setf lst (filter-by-bit lst i (most-common-bit lst i)))
           (when (= (length lst) 1)
             (return (bin->int (first lst))))))

(defun calc-co2 (lst)
  (loop for i from 0 below (num-bits lst)
        do (setf lst (filter-by-bit lst i (least-common-bit lst i)))
           (when (= (length lst) 1)
             (return (bin->int (first lst))))))

(defun parse-input (input) input)
(defun part1 (input) (* (calc-gamma input) (calc-epsilon input)))
(defun part2 (input) (* (calc-oxygen input) (calc-co2 input)))
