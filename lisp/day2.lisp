(defpackage :day2
  (:use :cl :trivia)
  (:export :run))

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

(defun run ()
  (let ((cmds (parse-input (util:read-input "day2"))))
    (format t "Part 1: ~a~%" (run-cmds cmds #'apply-cmd '(0 0)))
    (format t "Part 2: ~a~%" (run-cmds cmds #'apply-cmd2 '(0 0 0)))))
