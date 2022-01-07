(defpackage :day12
  (:use :cl :alexandria) 
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :->))

(in-package :day12)

(defun parse-input (lines)
  (loop for line in lines
        with m = (make-hash-table :test #'equal)
        finally (return m) do
          (destructuring-bind (a b) (str:split #\- line)
            (push b (gethash a m))
            (push a (gethash b m)))))

(defun is-small-cave (cave)
  "Return true if the given cave is a small cave. We don't consider
   the start and end caves small caves, since they are handled differently"
  (cond
    ((equal cave "start") nil)
    ((equal cave "end") nil)
    (t (every #'lower-case-p cave))))

(defun update-visited (visited cave)
  "We maintain a map of the number of times each small cave was visited.
   This function increments the visit count for a cave, if it is small."
  (if (is-small-cave cave) (cons cave visited) visited))

(defun count-duplicates (lst)
  "Return the number of duplicates in a list"
  (- (length lst)
     (length (remove-duplicates lst :test #'equal))))

(defun find-paths (graph src visited max-dups)
  (if (equal src "end")
      (list (list "end"))
      (let ((visited (update-visited visited src)))
        (when (<= (count-duplicates visited) max-dups)
          (->> (gethash src graph)
            (remove-if #'(lambda (c) (equal c "start")))
            (mapcar #'(lambda (c) (find-paths graph c visited max-dups)))
            (apply #'concatenate 'list)
            (mapcar #'(lambda (path) (cons src path))))))))

(defun part1 (graph)
  (length (find-paths graph "start" nil 0)))

(defun part2 (graph)
  (length (find-paths graph "start" nil 1)))
