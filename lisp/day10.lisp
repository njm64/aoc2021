(defpackage :day10
  (:use :cl :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> ->))

(in-package :day10)

(defun parse-expr (line)
  (let ((stack nil))
    (loop for tok across line do
      (case tok
        (#\{ (push #\} stack))
        (#\[ (push #\] stack))
        (#\< (push #\> stack))
        (#\( (push #\) stack))
        (t (when (not (eql (pop stack) tok))
             (return-from parse-expr (list :unexpected-token tok))))))
    (if (null stack)
        (list :ok)
        (list :unterminated-expr stack))))

(defun parse-input (lines)
  (mapcar #'parse-expr lines))

(defun unexpected-token-score (tok)
  (case tok
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)
    (t 0)))

(defun autocomplete-token-score (tok)
  (case tok
    (#\) 1)
    (#\] 2)
    (#\} 3)
    (#\> 4)
    (t 0)))

(defun autocomplete-score (toks)
  (reduce (lambda (acc tok)
            (+ (* acc 5)
               (autocomplete-token-score tok)))
          toks :initial-value 0))

(defun part1 (results)
  (loop for (status data) in results
        when (eql status :unexpected-token)
          sum (unexpected-token-score data)))

(defun part2 (results)
  (loop for (status data) in results
        when (eql status :unterminated-expr)
          collect (autocomplete-score data) into r
        finally (return (median r))))


