(defpackage :day16
  (:use :cl :trivia) 
  (:export :parse-input :part1 :part2))

(in-package :day16)

(defclass reader ()
  ((buf :initarg :buf)
   (pos :initform 0 :accessor pos)))

(defun parse-hex (line)
  (loop for i below (length line) by 2
        collect (parse-integer line :start i :end (+ i 2) :radix 16) into r
        finally (return (make-array (length r)
                                    :element-type '(unsigned-byte 8)
                                    :initial-contents r))))
(defun parse-input (lines)
  (parse-hex (first lines)))

(defun read-bit (r)
  (with-slots (buf pos) r
    (let* ((byte-offset (truncate (/ pos 8)))
           (bit-offset (- 7 (mod pos 8)))
           (mask (ash 1 bit-offset)))
      (incf pos)
      (ash (logand (aref buf byte-offset) mask) (- bit-offset)))))

(defun read-bits (r n)
  (loop repeat n with val = 0 finally (return val) do
    (setf val (logior (ash val 1) (read-bit r)))))

(defun get-version-sum (r)
  (let ((version (read-bits r 3))
        (type (read-bits r 3)))
    (if (= type 4)
        (loop                  
          (let ((ok (read-bit r)))
            (read-bits r 4)
            (when (zerop ok) (return))))
        (let ((length-type-id (read-bit r)))
          (if (zerop length-type-id)
              (let* ((n (read-bits r 15))
                     (end-pos (+ (pos r) n)))
                (loop while (< (pos r) end-pos)
                      do (setf version (+ version (get-version-sum r)))))
              (let ((packet-count (read-bits r 11)))
                (loop repeat packet-count
                      do (setf version (+ version (get-version-sum r))))))))
    version))

(defun cmp-gt (a b) (if (> a b) 1 0))
(defun cmp-lt (a b) (if (< a b) 1 0))
(defun cmp-eq (a b) (if (= a b) 1 0))

(defun parse-packet (r)
  "Convert a packet into a Lisp form so we can eval it directly"
  (let ((version (read-bits r 3))
        (type (read-bits r 3)))
    (declare (ignore version))
    (ecase type
      (0 (cons '+ (parse-list r)))
      (1 (cons '* (parse-list r)))
      (2 (cons 'min (parse-list r)))
      (3 (cons 'max (parse-list r)))
      (4 (parse-literal r))
      (5 (cons 'cmp-gt (parse-list r)))
      (6 (cons 'cmp-lt (parse-list r)))
      (7 (cons 'cmp-eq (parse-list r))))))

(defun parse-literal (r)
  (let ((n 0) (ok 1))
    (loop while (= ok 1) do
      (setf ok (read-bit r))
      (setf n (logior (ash n 4) (read-bits r 4))))
    n))

(defun parse-list (r)
  (let ((lst nil)
        (length-type-id (read-bit r)))
    (if (zerop length-type-id)
        (let* ((n (read-bits r 15))
               (end-pos (+ (pos r) n)))
          (loop while (< (pos r) end-pos) do
            (push (parse-packet r) lst)))
        (let ((packet-count (read-bits r 11)))
          (loop repeat packet-count do
            (push (parse-packet r) lst))))
    (nreverse lst)))

(defun part1 (data)
  (let ((r (make-instance 'reader :buf data)))
    (get-version-sum r)))

(defun part2 (data)
  (let* ((r (make-instance 'reader :buf data)))
    (eval (parse-packet r))))
