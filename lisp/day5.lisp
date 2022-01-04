(defpackage :day5
  (:use :cl) 
  (:export :parse-input :part1 :part2]))

(in-package :day5)

(defun parse-line (line)
  (cl-ppcre:register-groups-bind (x1 y1 x2 y2)
      ("(\\d+),(\\d+) -> (\\d+),(\\d+)" line)
    (mapcar #'parse-integer (list x1 y1 x2 y2))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun calc-map-dimensions (cmds)
  (loop for (x1 y1 x2 y2) in cmds
        maximizing x1 into mx
        maximizing x2 into mx
        maximizing y1 into my
        maximizing y2 into my
        finally (return (list (+ mx 1) (+ my 1)))))

(defun draw-horizontal (m y x1 x2)
  (loop for x from (min x1 x2) to (max x1 x2) do
    (incf (aref m x y))))

(defun draw-vertical (m x y1 y2)
  (loop for y from (min y1 y2) to (max y1 y2) do
    (incf (aref m x y))))

(defun draw-diagonal (m x1 y1 x2 y2)
  (let ((dx (abs (- x2 x1)))
        (dy (abs (- y2 y1)))
        (xs (if (> x2 x1) 1 -1))
        (ys (if (> y2 y1) 1 -1)))
    (when (not (= dx dy))
      (error "Line is not diagonal"))
    (do ((x x1 (+ x xs))
         (y y1 (+ y ys)))
        ((= x (+ x2 xs)))
      (incf (aref m x y)))))

(defun count-map (m)
  (destructuring-bind (w h) (array-dimensions m)
    (loop for x from 0 below w sum
      (loop for y from 0 below h count (> (aref m x y) 1)))))

(defun run-cmds (cmds &key allow-diagonal)
  (let ((m (make-array (calc-map-dimensions cmds))))
    (loop for (x1 y1 x2 y2) in cmds do
      (cond
        ((= y1 y2) (draw-horizontal m y1 x1 x2))
        ((= x1 x2) (draw-vertical m x1 y1 y2))
        (allow-diagonal (draw-diagonal m x1 y1 x2 y2))))
    (count-map m)))

(defun part1 (cmds) (run-cmds cmds))
(defun part2 (cmds) (run-cmds cmds :allow-diagonal t))

