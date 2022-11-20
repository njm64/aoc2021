(defpackage :day8
  (:use :cl :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> ->))

(in-package :day8)

(defparameter *all-keys* nil)
(map-permutations (lambda (p) (push p *all-keys*)) "abcdefg")

(defun parse-line (line)
  (->> line (str:split #\|) (mapcar #'str:words)))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun unique-code? (code)
  (member (length code) '(2 3 4 7)))

(defmacro make-mask (code)
  "Convert a segment id string into a bitmask at compile time"
  (loop for c across code sum (ash 1 (- (char-int c) (char-int #\a)))))

(defun mask->digit (m)
  (cond
    ((= m (make-mask "abcefg")) #\0)
    ((= m (make-mask "cf")) #\1)
    ((= m (make-mask "acdeg")) #\2)
    ((= m (make-mask "acdfg")) #\3)
    ((= m (make-mask "bcdf")) #\4)
    ((= m (make-mask "abdfg")) #\5)
    ((= m (make-mask "abdefg")) #\6)
    ((= m (make-mask "acf")) #\7)
    ((= m (make-mask "abcdefg")) #\8)
    ((= m (make-mask "abcdfg")) #\9)
    (t #\?)))

(defun decode (key code)
  "Decode a code using the given key"
  (loop for c across code
        with mask = 0 do
          (let* ((s (schar key (- (char-int c) (char-int #\a))))
                 (m (ash 1 (- (char-int s) (char-int #\a)))))
            (setf mask (logior mask m)))
        finally (return (mask->digit mask))))

(defun decode-codes (key codes)
  "Decode a list of codes into a string"
  (loop for code in codes
        collect (decode key code) into r
        finally (return (concatenate 'string r))))

(defun find-key (codes)
  "Find a key using the codes on the left hand side of an input line.
   For each possible key, we try to decode the code, and then sort
   the decoded digits. If it matches '0123456789' then we have the
   correct key."
  (find-if (lambda (k)
             (-> (decode-codes k codes)
               (sort #'char-lessp)
               (equal "0123456789")))
           *all-keys*))

(defun decode-record (record)
  "Find the key using the left hand side of a record, and use it to
   decode the digits on the right hand side."
  (destructuring-bind (a b) record
    (-> (find-key a)
      (decode-codes b)
      (parse-integer))))

(defun part1 (records)
  "Just count the codes with unique segment counts"
  (loop for (a b) in records
        sum (loop for s in b count (unique-code? s))))

(defun part2 (records)
  "Decode all records, and find the sum"
  (loop for r in records sum (decode-record r)))
