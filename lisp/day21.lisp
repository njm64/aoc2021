(defpackage :day21
  (:use :cl :alexandria)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day21)

(defun parse-player (line)
  "Parse a player into a list of (id, pos, score)"
  (cl-ppcre:register-groups-bind (id pos)
      ("Player (\\d+) starting position: (\\d+)" line)
    (when (and id pos)
      (list (parse-integer id) (parse-integer pos) 0))))

(defun player-id (player) (first player))
(defun player-score (player) (third player))

(defun parse-input (lines)
  "Parse input into a list of two players (active, inactive)"
  (let ((p1 (parse-player (first lines)))
        (p2 (parse-player (second lines))))
    (list p1 p2)))

(defun update-universe (universe roll)
  "Update the state of a universe with a given total die roll.
   swap the active and inactive players after each turn."
  (destructuring-bind (active inactive) universe
    (destructuring-bind (id pos score) active
      (let* ((new-pos (1+ (mod (+ pos roll -1) 10)))
             (new-score (+ score new-pos)))
        (list inactive (list id new-pos new-score))))))

(defparameter dice-totals
  (loop for d1 from 1 to 3 append
    (loop for d2 from 1 to 3 append
      (loop for d3 from 1 to 3 collect (+ d1 d2 d3)))))

(defun simplify-freq-list (lst)
  (loop for (key . freq) in lst
        with ht = (make-hash-table :test #'equal)
        finally (return (hash-table-alist ht))
        do (setf (gethash key ht) (+ freq (gethash key ht 0)))))

(defun has-won (universe)
  "Return true if a universe has reached the winning state for part 2."
  (let* ((inactive (second universe))
         (score (player-score inactive)))
    (>= score 21)))

(defun universe-permutations (universe)
  "Update a universe with all possible dice totals,
   and return a new list of universes."
  (destructuring-bind (u . count) universe
    (if (has-won u)
        (list universe)
        (mapcar (lambda (n) (cons (update-universe u n) count))
                dice-totals))))

(defun count-wins (universes id)
  "Count wins for the given player id, in a list of (universe,frequency)
   pairs. The winning player is always the player who has just rolled
   (i.e. the inactive player)"
  (loop for ((active inactive) . count) in universes
        when (= (player-id inactive) id)
          sum count))

(defun part1 (input)
  (loop for r = 0 then (+ r 3 ) with u = input do
    (let ((d1 (+ (mod r 100) 1))
          (d2 (+ (mod r 100) 2))
          (d3 (+ (mod r 100) 3)))
      (setf u (update-universe u (+ d1 d2 d3)))
      (destructuring-bind (active inactive) u
        (when (>= (player-score inactive) 1000)
          (return-from part1 (* (+ r 3) (player-score active))))))))

(defun part2 (input)
  (let ((universes (list (cons input 1))))
    (loop until (every (lambda (u) (has-won (first u))) universes) do
      (setf universes (->> universes
                        (mapcar #'universe-permutations)
                        (apply #'concatenate 'list)
                        (simplify-freq-list))))
    (max (count-wins universes 1)
         (count-wins universes 2))))


