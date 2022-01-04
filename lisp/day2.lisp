(defpackage :day2
  (:use :cl :trivia)
  (:export :parse-input :part1 :part2))

(in-package :day2)

(defun parse-cmd (line)
  (let* ((tokens (str:words line))
         (dir (util:make-keyword (first tokens)))
         (amount (parse-integer (second tokens))))
    (list dir amount)))

(defun parse-input (lines)
  (mapcar #'parse-cmd lines))

(defun apply-cmd (state cmd)
  (destructuring-bind (pos depth) state
    (match cmd
      ((list :forward n) (list (+ pos n) depth))
      ((list :down n) (list pos (+ depth n)))
      ((list :up n) (list pos (- depth n))))))

(defun apply-cmd2 (state cmd)
  (destructuring-bind (pos depth aim) state
    (match cmd
      ((list :forward n) (list (+ pos n) (+ depth (* aim n)) aim))
      ((list :down n) (list pos depth (+ aim n)))
      ((list :up n) (list pos depth (- aim n))))))

(defun run-cmds (cmds f state)
  (let ((r (reduce f cmds :initial-value state)))
    (* (first r) (second r))))

(defun part1 (cmds) (run-cmds cmds #'apply-cmd '(0 0)))
(defun part2 (cmds) (run-cmds cmds #'apply-cmd2 '(0 0 0)))


