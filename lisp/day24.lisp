(defpackage :day24
  (:use :cl :alexandria)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day24)

(defconstant +instructions-per-phase+ 18)

(defclass alu ()
  ((input :initform nil :initarg :input)
   (w :initform 0)
   (x :initform 0)
   (y :initform 0)
   (z :initform 0 :initarg :z)))

(defun parse-reg (s)
  (switch (s :test 'equal)
    ("w" 'w)
    ("x" 'x)
    ("y" 'y)
    ("z" 'z)
    (otherwise nil)))

(defun parse-operand (s)
  (or (parse-reg s)
      (parse-integer s)))

(defun parse-instruction (line)
  (destructuring-bind (op a &optional b) (str:words line)   
    (switch (op :test 'equal)
      ("inp" (list :inp (parse-reg a)))
      ("add" (list :add (parse-reg a) (parse-operand b)))
      ("mul" (list :mul (parse-reg a) (parse-operand b)))
      ("div" (list :div (parse-reg a) (parse-operand b)))
      ("mod" (list :mod (parse-reg a) (parse-operand b)))
      ("eql" (list :eql (parse-reg a) (parse-operand b))))))

(defun store (alu r v)
  (setf (slot-value alu r) v))

(defun get-reg (alu r)
  (slot-value alu r))

(defun get-val (alu r)
  (if (symbolp r)
      (get-reg alu r)
      r))

(defun execute (alu instruction)
  (destructuring-bind (op a &optional b) instruction
    (ecase op
      (:inp (store alu a (pop (slot-value alu 'input))))
      (:add (store alu a (+ (get-reg alu a) (get-val alu b))))
      (:mul (store alu a (* (get-reg alu a) (get-val alu b))))
      (:div (store alu a (floor (/ (get-reg alu a) (get-val alu b)))))
      (:mod (store alu a (mod (get-reg alu a) (get-val alu b))))
      (:eql (store alu a (if (= (get-reg alu a) (get-val alu b)) 1 0))))))

(defun instructions-for-phase (instructions n)
  (let ((start (* n +instructions-per-phase+)))
    (subseq instructions start (+ start +instructions-per-phase+))))

(defun run-phase (instructions d z)
  (let ((alu (make-instance 'alu :input (list d) :z z)))
    (loop for i in instructions do
      (execute alu i))
    (slot-value alu 'z)))

(defun calc-zs (instructions p zs)
  "Calculate the set of valid Z input values for phase p,
   given the set of valid Z output values zs."
  (let ((phase (instructions-for-phase instructions p))
        (output (make-hash-table)))
    (loop for d from 1 below 10 do
      (loop for z from 0 below 10000 do
        (let ((ret (run-phase phase d z)))
          (when (gethash ret zs)
            (setf (gethash z output) t)))))
    output))

(defun best-digit (instructions p z zs digits)
  "Find the best digit for phase p"
  (let ((instructions (instructions-for-phase instructions p)))
    (loop for d in digits do
      (let ((z (run-phase instructions d z)))
        (when (gethash z zs)
          (return-from best-digit (values d z)))))))

(defun calc (instructions zsets digits)
  (let ((z 0) (acc 0))
    (loop for i from 0 below 14 for zs in zsets do
      (multiple-value-bind (d next-z) (best-digit instructions i z zs digits)
        (setf z next-z)
        (setf acc (+ (* acc 10) d))))
    acc))

(defun calc-zsets (instructions)
  (let ((initial-set (make-hash-table)))
    (setf (gethash 0 initial-set) t)
    (let ((zsets (list initial-set)))
      (loop for i from 12 downto 0 do
        (push (calc-zs instructions (1+ i) (car zsets)) zsets))
      zsets)))

(defun parse-input (lines)
  (mapcar #'parse-instruction lines))

(defun part1 (instructions)
  (let ((zsets (calc-zsets instructions)))
    (calc instructions zsets (iota 9 :start 9 :step -1))))

(defun part2 (instructions)
  (let ((zsets (calc-zsets instructions)))
    (calc instructions zsets (iota 9 :start 1))))
