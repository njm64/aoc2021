(defpackage :day18
  (:use :cl :metabang-bind :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day18)

(defun check-char (c expected)
  (when (char-not-equal c expected)
    (error (format nil "Expected ~a" expected))))

(defun parse-node (s)
  (let ((c (read-char s)))
    (or (digit-char-p c)
        (progn
          (check-char c #\[)
          (let ((left (parse-node s)))
            (check-char (read-char s) #\,)
            (let ((right (parse-node s)))
              (check-char (read-char s) #\])
              (cons left right)))))))

(defun get-node (node path)
  (loop for dir in path do
    (setf node (if (eql dir :L) (car node) (cdr node))))
  node)

(defun set-node (node path val)
  (loop for subpath on path do
    (let ((dir (first subpath)))
      (if (consp (rest subpath))
          (setf node (if (eql dir :L) (car node) (cdr node)))
          (if (eql dir :L)
              (setf (car node) val)
              (setf (cdr node) val))))))

(defun get-next-int-path (node path)
  "Get the path for the next integer, or return nil if not found."
  ;; Start by reversing the path so we can easily trim from the end
  (let ((p (reverse path)))
    ;; Remove any right nodes from the end of the path
    (loop while (eql (first p) :R) do
      (setf p (rest p)))
    ;; If the path is empty now, we're done
    (when p
      ;; Otherwise remove the left node from the end of the path
      ;; and replace it with a right nodee
      (setf p (cons :R (rest p)))
      ;; Then we just need find the left-most tree node from this
      ;; point. Walk down the tree to the left, adding left nodes
      ;; until we hit a leaf node.
      (let ((n (get-node node (reverse p))))
        (loop while (consp n) do
          (setf n (car n))
          (setf p (cons :L p)))))
    ;; All done. Reverse the path so it's in the right order again.
    (nreverse p)))

(defun get-prev-int-path (node path)
  "Get the path for the previous integer, or return nil if not found."
  ;; Logic is just a mirror image of get-next-int-path
  (let ((p (reverse path)))
    (loop while (eql (first p) :L) do
      (setf p (rest p)))
    (when p
      (setf p (cons :L (rest p)))
      (let ((n (get-node node (reverse p))))
        (loop while (consp n) do
          (setf n (cdr n))
          (setf p (cons :R p)))))
    (nreverse p)))

(defun find-explode-path (node &optional path)
  (cond ((integerp node) nil)
        ((= (length path) 4) (reverse path))
        (t (or (find-explode-path (car node) (cons :L path))
               (find-explode-path (cdr node) (cons :R path))))))

(defun explode (node)
  "Attempt to explode a node. Return T if successful. Node is mutated."
  (when-let (path (find-explode-path node))
    (let ((val (get-node node path)))
      (when-let (prev-path (get-prev-int-path node path))
        (let ((prev-val (get-node node prev-path)))
          (set-node node prev-path (+ prev-val (car val))))) 
      (when-let (next-path (get-next-int-path node path))
        (let ((next-val (get-node node next-path)))
          (set-node node next-path (+ next-val (cdr val)))))
      (set-node node path 0)
      t)))

(defun find-split-path (node &optional path)
  (if (integerp node)
      (if (> node 9)
          (reverse path)
          nil)
      (or (find-split-path (car node) (cons :L path))
          (find-split-path (cdr node) (cons :R path)))))

(defun split (node)
  "Attempt to split a node. Return T if successful. Node is mutated."
  (when-let (path (find-split-path node))
    (let ((n (/ (get-node node path) 2)))
      (set-node node path (cons (floor n) (ceiling n)))
      t)))

(defun reduce-node (node)
  "Reduce a node by exploding and splitting. Node is mutated."
  (loop while (or (explode node) (split node))))

(defun add-numbers (a b)
  "Add two snailfish numbers together, return a new number."
  (let ((c (copy-tree (cons a b))))
    (reduce-node c)
    c))

(defun magnitude (num)
  (if (consp num)
      (+ (* 3 (magnitude (car num)))
         (* 2 (magnitude (cdr num))))
      num))

(defun parse-line (line)
  (with-input-from-string (s line)
    (parse-node s)))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun part1 (numbers)
  (let ((sum (reduce #'add-numbers numbers)))
    (magnitude sum)))

(defun part2 (numbers)
  (loop for a in numbers
        maximize
        (loop for b in numbers
              when (not (eq a b))
                maximize (magnitude (add-numbers a b)))))
