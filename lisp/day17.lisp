(defpackage :day17
  (:use :cl :metabang-bind :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day17)

(defun parse-input (lines)
  "Parse into an array of x1,x2,y1,y2"
  (->> (first lines)
    (str:replace-using (list  "target area: x=" "" " y=" "" ".." ","))
    (str:split #\,)
    (mapcar #'parse-integer)))

(defun run-simulation (target vx vy)
  (bind (((x1 x2 y1 y2) target)
         (x 0) (y 0)
         (max-height 0))
    (loop
      (setf x (+ x vx))
      (setf y (+ y vy))
      (cond
        ((> vx 0) (decf vx))
        ((< vx 0) (incf vx)))
      (decf vy)
      (setf max-height (max y max-height))
      (when (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))
        (return max-height))
      (when (or
             ;; Past max x, assuming target is to the right
             (> x x2)
             ;; Stopped moving horizontally, outside the target range
             (and (= vx 0) (or (< x x1) (> x x2)))
             ;; We're moving down, and are outside the target range
             (and (< vy 0) (< y y1)))
        (return nil)))))

(defun find-hits (target)
  (bind (((_ x2 _ _) target)
         (max-height 0) (hit-count 0))
    (loop for vx from 0 to x2 do
      (loop for vy from -1000 to 1000 do
        (when-let (h (run-simulation target vx vy))
          (setf max-height (max h max-height))
          (incf hit-count))))
    (list max-height hit-count)))

(defun part1 (target)
  (first (find-hits target)))

(defun part2 (target)
  (second (find-hits target)))
