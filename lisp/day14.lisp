(defpackage :day14
  (:use :cl :alexandria :trivia :split-sequence) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-> :-<>> :<>))

(in-package :day14)

(defun parse-rule (line)
  "Parse a rule into a tuple of the form ((A, B), C)"
  (match (str:words line)
    ((list key "->" data)
     (cons (cons (char key 0) (char key 1))
           (char data 0)))))

(defun parse-input (lines)
  "Parse input into a polymer string and a map of insertion rules"
  (destructuring-bind (a b) (split-sequence-if #'str:empty? lines)
    (list (first a) (-> (mapcar #'parse-rule b)
                      (alist-hash-table :test #'equal)))))

(defun polymer-to-pair-frequencies (s)
  "Split a polymer string into a list of pairs with frequencies.
   Initially the frequency of every pair will be 1."
  (loop for c across s with prev
        when prev
          collect (cons (cons prev c) 1)
        do (setf prev c)))

(defun simplify-frequency-list (lst)
  (loop for (key . freq) in lst
        with ht = (make-hash-table :test #'equal)
        finally (return (hash-table-alist ht))
        do (setf (gethash key ht) (+ freq (gethash key ht 0)))))

(defun apply-rules (rules pair-frequencies)
  (loop for (pair . count) in pair-frequencies
        with result = nil
        finally (return (nreverse result)) do
    (destructuring-bind (a . b) pair
      (let ((s (gethash pair rules)))
        (push (cons (cons a s) count) result)
        (push (cons (cons s b) count) result)))))

(defun get-element-counts (pair-frequencies)
  "Given a list of pair frequencies, return the characters with the
   minimum and maximum counts. We include the count of the second
   character in every pair, and the count of the first character
   in the first pair only."
  (let* ((hd (first pair-frequencies))
         (counts (-<>> pair-frequencies
                   (mapcar #'(lambda (pf) (cons (cdar pf) (cdr pf))))
                   (cons (cons (caar hd) (cdr hd)))
                   (simplify-frequency-list)
                   (mapcar #'cdr)
                   (sort <> #'<))))
    (list (first counts) (car (last counts)))))

(defun simplify (pair-frequencies)
  "Simplify a list of pair frequencies. Make sure the first pair is not
   simplified, because the ordering is important (i.e. we count both
   elements of the first pair). So remove it before simplifying,
   and re-add it afterwards."
  (cons (first pair-frequencies)
        (simplify-frequency-list (rest pair-frequencies))))

(defun run-iterations (pf rules n)
  (loop repeat n do (setf pf (simplify (apply-rules rules pf))))
  (destructuring-bind (min-count max-count) (get-element-counts pf)
    (- max-count min-count)))

(defun part1 (input)
  (destructuring-bind (polymer rules) input
    (let ((pair-frequencies (polymer-to-pair-frequencies polymer)))
      (run-iterations pair-frequencies rules 10))))

(defun part2 (input)
  (destructuring-bind (polymer rules) input
    (let ((pair-frequencies (polymer-to-pair-frequencies polymer)))
      (run-iterations pair-frequencies rules 40))))
