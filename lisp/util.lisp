(defpackage :util
  (:use :cl)
  (:export :read-input :make-keyword))

(in-package :util)

(defun read-input (name)
  (let ((filename (str:concat "../input/" name ".txt")))
    (uiop:read-file-lines filename)))

(defun make-keyword (name)
  (values (intern (string-upcase name) :keyword)))
