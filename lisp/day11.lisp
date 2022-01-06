(defpackage :day11
  (:use :cl :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> ->))

(in-package :day11)

(defun parse-line (line)
  (map 'list #'digit-char-p line))

(defun parse-input (lines)
  (let* ((lst (mapcar #'parse-line lines))
         (width (length (first lst)))
         (height (length lst)))
    (make-array (list height width) :initial-contents lst)))

(defun for-all (m f)
  (destructuring-bind (h w) (array-dimensions m)
    (loop for x below w do
      (loop for y below h do
        (funcall f m x y)))))

(defun for-neighbours (m x y f)
  (destructuring-bind (h w) (array-dimensions m)
    (loop for xi from (- x 1) to (+ x 1) do
      (loop for yi from (- y 1) to (+ y 1) do
        (when (and (>= xi 0) (>= yi 0) (< xi w) (< yi h))
          (funcall f m xi yi))))))

(defun inc-energy (m x y)
  (let ((e (aref m y x)))
    (when (< e 10)
      (incf (aref m y x)))
    (when (= e 9)
      (for-neighbours m x y #'inc-energy))))

(defun check-reset (m x y)
  (when (= (aref m y x) 10)
    (setf (aref m y x) 0)))

(defun count-flashes (m)
  (loop for i from 0 below (array-total-size m)
        count (= (row-major-aref m i) 10)))

(defun iterate (m)
  (for-all m #'inc-energy)
  (let ((n (count-flashes m)))
    (for-all m #'check-reset)
    n))

(defun part1 (m)
  (let ((m (copy-array m)))
    (loop repeat 100 sum (iterate m))))

(defun part2 (m)
  (let ((m (copy-array m))
        (total (array-total-size m)))
    (loop for i from 1
          until (= (iterate m) total)
          finally (return i))))
