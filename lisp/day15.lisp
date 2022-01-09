(defpackage :day15
  (:use :cl) 
  (:export :parse-input :part1 :part2))

(in-package :day15)

;; The risk table stores nil if a node hasn't yet been visited,
;; an integer risk value if it has, or +risk-closed+ if it is
;; closed (i.e. we've explored all its neighbours)
(defconstant +risk-closed+ -1)

(defun parse-line (line)
  (map 'list #'digit-char-p line))

(defun parse-input (lines)
  (let* ((lst (mapcar #'parse-line lines))
         (width (length (first lst)))
         (height (length lst)))
    (make-array (list height width) :initial-contents lst)))

(defun neighbours (cave pos)
  (destructuring-bind (x y) pos
    (destructuring-bind (h w) (array-dimensions cave)
      (let ((r nil))
        (when (> x 0) (push (list (1- x) y) r))
        (when (> y 0) (push (list x (1- y)) r))
        (when (< x (1- w)) (push (list (1+ x) y) r))
        (when (< y (1- h)) (push (list x (1+ y)) r))
        r))))

(defun set-pos (a pos val)
  (destructuring-bind (x y) pos
    (setf (aref a y x) val))) ;; Transpose x and y, so they are in row major order

(defun get-pos (a pos)
  (destructuring-bind (x y) pos
    (aref a y x))) ;; Transpose x and y, so they are in row major order

(defun find-path (cave src dst)
  (let* ((ra (make-array (array-dimensions cave) :initial-element nil))
         (q (make-instance 'cl-heap:priority-queue)) )
    (cl-heap:enqueue q src 0)
    (set-pos ra src 0)
    (loop
      (let* ((pos (cl-heap:dequeue q))
             (risk (get-pos ra pos)))
        (when (equal pos dst)
          (return risk))
        (unless (eql risk +risk-closed+)
          (loop for npos in (neighbours cave pos) do
            (let ((nrisk (get-pos ra npos))
                  (newrisk (+ risk (get-pos cave npos))))
              (when (or (null nrisk) (< newrisk nrisk))
                (set-pos ra npos newrisk)
                (cl-heap:enqueue q npos newrisk))))
          (set-pos ra pos +risk-closed+))))))

(defun find-std-path (cave)
  (destructuring-bind (h w) (array-dimensions cave)
    (let ((src (list 0 0)) (dst (list (1- w) (1- h))))
      (find-path cave src dst))))

(defun expand-cave (cave)
  (destructuring-bind (h w) (array-dimensions cave)
    (let* ((width (* w 5))
          (height (* h 5))
          (newcave (make-array (list width height))))
      (loop for x below width do
        (loop for y below height do
          (let* ((risk (aref cave (mod x w) (mod y w)))
                 (inc (+ (truncate (/ x w)) (truncate (/ y h))))
                 (val (1+ (mod (1- (+ risk inc)) 9)) ))
            (setf (aref newcave x y) val))))
      newcave)))

(defun part1 (cave) (find-std-path cave))
(defun part2 (cave) (find-std-path (expand-cave cave)))
