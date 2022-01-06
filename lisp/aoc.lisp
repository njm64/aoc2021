(defpackage :aoc
  (:use :cl)
  (:export :run :run-all))

(in-package :aoc)

(defun package-for-day (d)
  (find-package (format nil "DAY~d" d)))

(defun run-part (d p f input)
  (let* ((t1 (get-internal-run-time))
         (result (funcall f input))
         (t2 (get-internal-run-time))
         (elapsed (/ (- t2 t1) internal-time-units-per-second)))
    (format t "Day ~2d Part ~d: ~20@<~d~> ~,6fs~%" d p result elapsed)))

(defun run (d)
  (let* ((package (or (package-for-day d)
                      (error "Not implemented")))
         (filename (format nil "../input/day~d.txt" d))
         (lines (uiop:read-file-lines filename))
         (parse-input (or (find-symbol "PARSE-INPUT" package)
                          (error "Missing parse-input")))
         (input (funcall parse-input lines))
         (part1 (find-symbol "PART1" package))
         (part2 (find-symbol "PART2" package)))
    (when part1 (run-part d 1 part1 input))
    (when part2 (run-part d 2 part2 input))))

(defun run-all ()
  (loop for d from 1 to 25 do
    (when (package-for-day d)
      (run d))))
