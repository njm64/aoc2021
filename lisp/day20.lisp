(defpackage :day20
  (:use :cl :alexandria :split-sequence)
  (:export :parse-input :part1 :part2))

(in-package :day20)

(defclass image ()
  ((pixels :initarg :pixels)
   (background :initarg :background)))

(defun parse-input (lines)
  (destructuring-bind (a b) (split-sequence-if #'str:empty? lines)
    (let* ((iea (apply #'concatenate 'string a))
           (width (length (first b)))
           (height (length b))
           (pixels (make-array (list height width) :initial-contents b))
           (image (make-instance 'image :pixels pixels :background #\.)))
      (list iea image))))

(defun image-size (image)
  (destructuring-bind (h w) (array-dimensions (slot-value image 'pixels))
    (list w h)))

(defun get-pixel (image x y)
  (destructuring-bind (w h) (image-size image)
    (let ((c (if (and (>= x 0) (< x w) (>= y 0) (< y h))
                 (aref (slot-value image 'pixels) y x)
                 (slot-value image 'background))))
      (if (equal c #\#) 1 0))))

(defun iea-index (image x y)
  (logior
   (ash (get-pixel image (1- x) (1- y)) 8)
   (ash (get-pixel image x      (1- y)) 7)
   (ash (get-pixel image (1+ x) (1- y)) 6)
   (ash (get-pixel image (1- x) y)      5)
   (ash (get-pixel image x      y)      4)
   (ash (get-pixel image (1+ x) y)      3)
   (ash (get-pixel image (1- x) (1+ y)) 2)
   (ash (get-pixel image x      (1+ y)) 1)
   (ash (get-pixel image (1+ x) (1+ y)) 0)))

(defun enhance (iea src) 
  (destructuring-bind (w h) (image-size src)
    (let* ((new-height (+ h 2))
           (new-width (+ w 2))
           (new-pixels (make-array (list new-height new-width)))
           (background (char iea (iea-index src -2 -2))))
      (loop for x below new-width do
        (loop for y below new-height do
          (let ((i (iea-index src (1- x) (1- y))))
            (setf (aref new-pixels y x) (char iea i)))))
      (make-instance 'image :pixels new-pixels :background background))))

(defun count-pixels (image)
  (destructuring-bind (w h) (image-size image)
    (loop for y below h
          summing
          (loop for x below w
                summing
                (get-pixel image x y)))))

(defun run-iterations (iea image n)
  (loop repeat n do
    (setf image (enhance iea image)))
  (count-pixels image))

(defun part1 (input)
  (destructuring-bind (iea image) input
    (run-iterations iea image 2)))

(defun part2 (input)
  (destructuring-bind (iea image) input
    (run-iterations iea image 50)))


