(defpackage :day13
  (:use :cl :alexandria :trivia :split-sequence) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-> :-<>> :<>))

(in-package :day13)

(defun parse-point (line)
  (->> line
    (str:split #\,)
    (mapcar #'parse-integer)
    (apply #'cons)))

(defun parse-fold (line)
  (match (str:split #\= line)
    ((list "fold along x" s) (cons :vertical (parse-integer s)))
    ((list "fold along y" s) (cons :horizontal (parse-integer s)))))

(defun parse-input (lines)
  (destructuring-bind (points folds) (split-sequence-if #'str:empty? lines)
    (list (mapcar #'parse-point points)
          (mapcar #'parse-fold folds))))

(defun fold-point (p fold)
  (destructuring-bind (x . y) p
    (match fold
      ((cons :vertical n) (if (> x n) (cons (- n (- x n)) y) p))
      ((cons :horizontal n) (if (> y n) (cons x (- n (- y n))) p)))))

(defun fold-points (points fold)
  (-<>> points  
    (mapcar (lambda (p) (fold-point p fold)))
    (remove-duplicates <> :test #'equal)))

(defun plot-points (points)
  (let* ((width (1+ (apply #'max (mapcar #'car points))))
         (height (1+ (apply #'max (mapcar #'cdr points))))
         (arr (make-array (list height width) :initial-element #\Space)))
    (loop for (x . y) in points do (setf (aref arr y x) #\*))
    (loop for y below height do
      (loop for x below width do
        (write-char (aref arr y x)))
      (terpri))))

(defun part1 (inputs)
  (destructuring-bind (points folds) inputs
    (length  (fold-points points (first folds)))))

(defun part2 (inputs)
  (destructuring-bind (points folds) inputs
    (plot-points (reduce #'fold-points folds :initial-value points))))
