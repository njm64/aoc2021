(defpackage :aoc
  (:use :cl)
  (:export :run :run-all))

(in-package :aoc)

(defun package-for-day (d)
  (or (find-package (format nil "DAY~d" d))
      (error "Not implemented")))

(defun read-raw-input (d)
  (let ((filename (format nil "../input/day~d.txt" d)))
    (uiop:read-file-lines filename)))

(defun run-part (d p f input)
  (let* ((t1 (get-internal-run-time))
         (result (funcall f input))
         (t2 (get-internal-run-time))
         (elapsed (/ (- t2 t1) internal-time-units-per-second)))
    (format t "Day ~2d Part ~d: ~20@<~d~> ~,6fs~%" d p result elapsed)))


(defun read-input (d)
  (let* ((package (package-for-day d))
         (parse-input (or (find-symbol "PARSE-INPUT" package)
                          (error "Missing parse-input"))))
    (funcall parse-input (read-input d))))


(defun run (d)
  (let* ((package (package-for-day d))
         (input (read-input d))
         (part1 (find-symbol "PART1" package))
         (part2 (find-symbol "PART2" package)))
    (when part1 (run-part d 1 part1 input))
    (when part2 (run-part d 2 part2 input))))

(defun run-all ()
  (loop for d from 1 to 25 do
    (when (package-for-day d)
      (run d))))
