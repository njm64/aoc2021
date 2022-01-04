(defpackage :day6
  (:use :cl) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day6)

(defun parse-input (lines)
  "Parse input into a list of (timer . count) cons cells"
  (->> (first lines)
    (str:split #\,)
    (mapcar #'parse-integer)
    (mapcar #'(lambda (n) (cons n 1)))))

(defun count-fish (fish)
  (apply #'+ (mapcar #'cdr fish)))

(defun count-time (fish time)
  (loop for (ti . count) in fish when (= time ti) sum count))

(defun group (fish)
  (loop for ti from 0 to 9 collect (cons ti (count-time fish ti))))

(defun iterate (fish)
  (loop for (timer . count) in fish
        if (zerop timer) collect (cons 6 count) and collect (cons 8 count)
        else collect (cons (1- timer) count)))

(defun run-simulation (fish n)
  (loop repeat (1+ n)
        for f = fish then (group (iterate f))
        finally (return (count-fish f))))

(defun part1 (fish) (run-simulation fish 80))
(defun part2 (fish) (run-simulation fish 256))


