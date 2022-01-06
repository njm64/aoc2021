(defpackage :day9
  (:use :cl :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> ->))

(in-package :day9)

(defun parse-line (line)
  (map 'list #'digit-char-p line))

(defun parse-input (lines)
  (let* ((lst (mapcar #'parse-line lines))
         (width (length (first lst)))
         (height (length lst)))
    (make-array (list height width) :initial-contents lst)))

(defun get-height (m x y)
  "Return the height for an (x,y) position on the map. If the position
   is out of bounds, return the maximum height 9."
  (destructuring-bind (h w) (array-dimensions m)
    (if (and (>= x 0) (>= y 0) (< x w) (< y h))
        (aref m y x)
        9)))

(defun basin-size (m x y)
  "Get the basin size at an (x,y) coordinate. Note this mutates the map."
  (if (=  (get-height m x y) 9) 0
      (progn
        ;; Set the height at this point to 9 so we won't consider it again
        (setf (aref m y x) 9)
        ;; Recursively calculate the size of the basin
        (+ (basin-size m (- x 1) y)
           (basin-size m (+ x 1) y)
           (basin-size m x (- y 1))
           (basin-size m x (+ y 1))
           1))))

(defun basin-sizes (m)
  (destructuring-bind (height width) (array-dimensions m)
    (let ((sizes nil))
      (loop for x from 0 below width do
        (loop for y from 0 below height do
          (let ((size (basin-size m x y)))
            (when (> size 0)
              (push size sizes)))))
      sizes)))

(defun part1 (m)
  "Calculate the total risk for the height map"
  (destructuring-bind (height width) (array-dimensions m)
    (let ((risk 0))
      (loop for x from 0 below width do
        (loop for y from 0 below height do
          (let ((h (get-height m x y)))
            (when (and (< h (get-height m (- x 1) y))
                       (< h (get-height m (+ x 1) y))
                       (< h (get-height m x (- y 1)))
                       (< h (get-height m x (+ y 1))))
              (setf risk (+ risk h 1))))))
      risk)))

(defun part2 (m)
  "Calculate the product of the three largest basin sizes"
  (let ((sizes (-> (basin-sizes m) (sort #'>) (subseq 0 3))))
    (apply #'* sizes)))

