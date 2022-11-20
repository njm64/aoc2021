(defpackage :day22
  (:use :cl :alexandria)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day22)

(defun parse-cmd (line)
  (cl-ppcre:register-groups-bind (status x1 x2 y1 y2 z1 z2)
      ("(on|off) x=(.*)\\.\\.(.*),y=(.*)\\.\\.(.*),z=(.*)\\.\\.(.*)" line)
    (when (and status x1 x2 y1 y2 z1 z2)
      (list (if (equal status "on") 1 0)
            (list (cons (parse-integer x1) (1+ (parse-integer x2)))
                  (cons (parse-integer y1) (1+ (parse-integer y2)))
                  (cons (parse-integer z1) (1+ (parse-integer z2))))))))

(defun parse-input (lines)
  (mapcar #'parse-cmd lines))

(defun in-init-region (cmd)
  (destructuring-bind (status ((x1 . x2) (y1 . y2) (z1 . z2))) cmd
    (declare (ignore status))
      (and (>= x1 -50) (<= x2 50)
           (>= y1 -50) (<= y2 50)
           (>= z1 -50) (<= z2 50))))

(defun make-axis (cmds i)
  (-<>> cmds
    (mapcar (lambda (cmd)
              (let ((r (nth i (second cmd))))
                (list (car r) (cdr r)))))
    (apply #'concatenate 'list)
    (sort <> #'<)
    (remove-duplicates)
    (coerce <> '(vector fixnum))))

(defun map-range (axis range)
  (destructuring-bind (min . max) range
    (cons (position min axis) (position max axis))))

(defun run-cmds (cmds)
  (let* ((x-axis (make-axis cmds 0))
         (y-axis (make-axis cmds 1))
         (z-axis (make-axis cmds 2))
         (xs (length x-axis))
         (ys (length y-axis))
         (zs (length z-axis))
         (reactor (make-array (list xs ys zs) :element-type 'bit))
         (count 0))
    (loop for (status (xr yr zr)) in cmds do
      (let ((xr (map-range x-axis xr))
            (yr (map-range y-axis yr))
            (zr (map-range z-axis zr)))
        (loop for x from (car xr) below (cdr xr) do
          (loop for y from (car yr) below (cdr yr) do
            (loop for z from (car zr) below (cdr zr) do
              (setf (aref reactor x y z) status))))))
    (loop for z from 0 below zs do
      (loop for y from 0 below ys do
        (loop for x from 0 below xs do
          (when (= (aref reactor x y z) 1)
            (let ((xc (- (aref x-axis (1+ x)) (aref x-axis x)))
                  (yc (- (aref y-axis (1+ y)) (aref y-axis y)))
                  (zc (- (aref z-axis (1+ z)) (aref z-axis z))))
              (setf count (+ count (* xc yc zc))))))))
    count))

(defun part1 (input)
  (run-cmds (remove-if-not #'in-init-region input)))

(defun part2 (input)
  (run-cmds input))


