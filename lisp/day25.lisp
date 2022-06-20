(defpackage :day25
  (:use :cl :alexandria)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->))

(in-package :day25)

(defun parse-input (lines)
  (let ((width (length (first lines)))
        (height (length lines)))
    (make-array (list height width) :initial-contents lines)))

(defun move (map c dx dy)
  (destructuring-bind (h w) (array-dimensions map)
    (let ((m (make-array (list h w) :initial-element #\.)))
      (loop for x from 0 below w do
        (let ((x2 (mod (+ x dx) w)))
          (loop for y from 0 below h do
            (let ((y2 (mod (+ y dy) h)) (p (aref map y x)))
              (cond
                ((and (eql p c) (eql (aref map y2 x2) #\.))
                 (setf (aref m y2 x2) c))
                ((not (eql p #\.))
                 (setf (aref m y x) p)))))))
      m)))

(defun iterate (map)
  (loop for count = 1 then (1+ count) do
    (let ((m (-> map (move #\> 1 0) (move #\v 0 1))))
      (if (equalp m map)
          (return count)
          (setf map m)))))

(defun part1 (map) (iterate map))
(defun part2 (map) (declare (ignore map)) 0)
