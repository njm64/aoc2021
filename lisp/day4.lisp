(defpackage :day4
  (:use :alexandria :cl :split-sequence)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day4)

(defun parse-line (line)
  (mapcar #'parse-integer (str:words line)))

(defun parse-board (lines)
  (let* ((data (mapcar #'parse-line lines))
         (h (length data))
         (w (length (first data))))
    (make-array (list h w) :initial-contents data)))

(defun parse-input (lines)
  (list (->> (first lines)
          (str:split #\,)
          (mapcar #'parse-integer))
        (->> (subseq lines 2)
          (split-sequence-if #'str:empty?)
          (mapcar #'parse-board))))

(defun mark-board (board n)
  (destructuring-bind (h w) (array-dimensions board)
    (loop for x from 0 below w do
      (loop for y from 0 below h do
        (when (eql (aref board y x) n)
          (setf (aref board y x) nil))))))

(defun is-complete (board)
  (destructuring-bind (h w) (array-dimensions board)
    (loop for x from 0 below w do
      (when (loop for y from 0 below h never (aref board y x))
        (return-from is-complete t)))
    (loop for y from 0 below h do
      (when (loop for x from 0 below w never (aref board y x))
        (return-from is-complete t)))))

(defun find-bingos (numbers boards-in)
  (let ((boards (mapcar #'copy-array boards-in)))
    (loop
      for n in numbers while boards with completed
      finally (return (nreverse completed)) do
        (loop for b in boards do            
          (mark-board b n)
          (when (is-complete b)
            (push (list b n) completed)))
        (setf boards (remove-if #'is-complete boards)))))

(defun calc-score (board num)
  (destructuring-bind (h w) (array-dimensions board)
    (let ((sum 0))
      (loop for x from 0 below w do
        (loop for y from 0 below h do
          (when-let (n (aref board y x))
            (setf sum (+ sum n)))))
      (* sum num))))

(defun part1 (input)
  (destructuring-bind (numbers boards) input
    (apply #'calc-score (first (find-bingos numbers boards)))))

(defun part2 (input)
  (destructuring-bind (numbers boards) input
    (apply #'calc-score (lastcar (find-bingos numbers boards)))))


