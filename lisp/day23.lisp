(defpackage :day23
  (:use :cl :alexandria :metabang-bind)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day23)

(defparameter *col-amber* 2)
(defparameter *col-bronze* 4)
(defparameter *col-copper* 6)
(defparameter *col-desert* 8)
(defparameter *columns* (list *col-amber* *col-bronze* *col-copper* *col-desert*))
(defparameter *row-hallway* 0)
(defparameter *row-room-top* 1)
(defparameter *row-room-bottom* 2)
(defparameter *hallway-length* 11)

(defparameter *energy-closed* -1)

(defclass search-state ()
  ((energy :initarg :energy :reader state-energy)
   (map :initarg :map :reader state-map)
   (amphipods :initarg :amphipods :reader state-amphipods)))

(defun parse-input (lines)
  (loop for row from 1 to 2
        append
        (loop for col in *columns*
              with line = (nth (1+ row) lines)
              collect (list (char line (1+ col)) (cons row col)))))

(defun map-width () (1+ *hallway-length*))
(defun map-height () (1+ *row-room-bottom*))

(defun room-rows ()
  (loop for r from *row-room-bottom* downto *row-room-top* collect r))

(defun hallway-dsts ()
  (mapcar (lambda (col) (cons 0 col))
          (list 0 1 3 5 7 9 10)))

(defun col-for-kind (kind)
  (ecase kind
    (#\A *col-amber*)
    (#\B *col-bronze*)
    (#\C *col-copper*)
    (#\D *col-desert*)))

(defun room-for-pos (pos)
  (destructuring-bind (row . col) pos
    (cond
      ((= row *row-hallway*) nil)
      ((= col *col-amber*) #\A)
      ((= col *col-bronze*) #\B)
      ((= col *col-copper*) #\C)
      ((= col *col-desert*) #\D))))

(defun cost-for-kind (kind)
  (ecase kind
    (#\A 1)
    (#\B 10)
    (#\C 100)
    (#\D 1000)))

(defun build-map (amphipods)
  (let* ((w (map-width))
         (h (map-height))
         (s (make-string (* w h) :initial-element #\#)))
    (loop for i from 0 below h do
      (setf (char s (+ (* w i) w -1)) #\linefeed))
    (loop for i from 0 below *hallway-length* do
      (setf (char s i) #\.))
    (loop for i from *row-room-top* to *row-room-bottom* do
      (setf (char s (+ (* i w) *col-amber*)) #\.)
      (setf (char s (+ (* i w) *col-bronze*)) #\.)
      (setf (char s (+ (* i w) *col-copper*)) #\.)
      (setf (char s (+ (* i w) *col-desert*)) #\.))
    (loop for (kind (row . col)) in amphipods do
      (setf (char s (+ (* row w) col)) kind))
    s))

(defun map-get (m pos)
  "Get the character at the given map position"
  (destructuring-bind (row . col) pos
    (char m (+ col (* row (map-width))))))

(defun is-pos-clear (m pos)
  "Return T if the given position is unoccupied"
  (equal (map-get m pos) #\.))

(defun is-path-clear (m path)
  "Return T is all positions in the given path are clear"
  (every (lambda (pos) (is-pos-clear m pos)) path))

(defun room-positions (kind)
  "Return a list of app positions in a room, ordered from bottom to top"
  (let ((col (col-for-kind kind)))
    (mapcar (lambda (r) (cons r col)) (room-rows))))

(defun empty-room-position (m kind)
  "Find the first clear position in a room, from the bottom up"
  (find-if (lambda (pos) (is-pos-clear m pos)) (room-positions kind)))

(defun strangers-in-room (m kind)
  "Return T if there are any strangers in the room for a kind"
  (notevery (lambda (pos)
              (or (is-pos-clear m pos)
                  (equal (map-get m pos) kind)))
            (room-positions kind)))

(defun hpath (row src-col dst-col)
  (if (< src-col dst-col)
      (loop for col from (1+ src-col) upto dst-col collect (cons row col))
      (loop for col from (1- src-col) downto dst-col collect (cons row col))))

(defun vpath (col src-row dst-row)
  (if (< src-row dst-row)
      (loop for row from (1+ src-row) upto dst-row collect (cons row col))
      (loop for row from (1- src-row) downto dst-row collect (cons row col))))

(defun get-path (src dst)
  "Generate a path from source to destination"
  (destructuring-bind (src-row . src-col) src
    (destructuring-bind (dst-row . dst-col) dst
      (cond
        ;; Moving up or down within the same room
        ((and (= src-col dst-col) (/ src-row dst-row ))
         (vpath src-col src-row dst-row))
        ;; Room to hallway
        ((and (/ src-row *row-hallway*) (= dst-row *row-hallway*))
         (nconc (vpath src-col src-row *row-hallway*)
                (hpath *row-hallway* src-col dst-col)))
        ;; Hallway to room
        ((and (= src-row *row-hallway*) (/ dst-row *row-hallway*))
         (nconc (hpath *row-hallway* src-col dst-col)
                (vpath dst-col src-row dst-row)))
        ;; Room to room
        ((and (/ src-col dst-col) (/ src-row *row-hallway*) (/ dst-row *row-hallway*))
         (nconc (vpath src-col src-row *row-hallway*)
                (hpath *row-hallway* src-col dst-col)
                (vpath dst-col *row-hallway* dst-row)))))))

(defun valid-destinations (m a)
  (destructuring-bind (kind pos) a
    (let ((strangers (strangers-in-room m kind))
          (room (room-for-pos pos)))
      (cond
        ;; Strangers in our room, and we're in the hallway. Can't go anywhere.
        ((and strangers (not room))
         nil)
        ;; Strangers in our room, and we're in a room. We can only go
        ;; to the hallway. If we're in our room, we're allowed to leave if
        ;; there are strangers there, as we may need to get out of the
        ;; way to allow them to leave.
        ((and strangers room)
         (hallway-dsts))
        ;; No strangers in our room, and we're in the hallway.
        ;; We can go to our room.
        ((and (not strangers) (not room))
         (list (empty-room-position m kind)))
        ;; No strangers in our room, and we're in it. No need to go anywhere.
        ((and (not strangers) (equal room kind))
         nil)
        ;; No strangers in our room, and we're in a different room.
        ;; We can go either to our room, or to the hallway.
        ((and (not strangers) room)
         (cons (empty-room-position m kind) (hallway-dsts)))
        (t (error "No valid desinations"))))))

(defun get-new-states (s a)
  (with-slots (map energy amphipods) s
    (destructuring-bind (kind pos) a
      (loop for dst in (valid-destinations map a)
            for path = (get-path pos dst)
            when (and path (is-path-clear map path))
              collect
              (let ((new-amphipods (subst (list kind dst) a amphipods)))
                (make-instance 'search-state
                               :energy (+ energy (* (cost-for-kind kind) (length path)))
                               :amphipods new-amphipods
                               :map (build-map new-amphipods)))))))

(defun is-complete (s)
  "Return true if all amphipods are in their home rooms"
  (every (lambda-bind ((kind pos)) (equal (room-for-pos pos) kind))
         (state-amphipods s)))

(defun is-closed (ht s)
  (equal (gethash (state-map s) ht) *energy-closed*))

(defun should-process-state (ht s)
  "Return true if we should process this state
   (i.e. if we haven't already reached it with less energy)"
  (let ((e (gethash (state-map s) ht)))
    (or (not e) (< (state-energy s) e))))

(defun find-solution (initial-state)
  (let ((q (make-instance 'cl-heap:priority-queue))
        (ht (make-hash-table :test #'equal)))
    (cl-heap:enqueue q initial-state 0)
    (loop for s = (cl-heap:dequeue q) while s do
      (when (is-complete s)
        (return-from find-solution (state-energy s)))
      (when (not (is-closed ht s))
        (loop for a in (state-amphipods s) do
          (loop for new-state in (get-new-states s a) do
            (when (should-process-state ht new-state)
              (setf (gethash (state-map new-state) ht) (state-energy new-state))
              (cl-heap:enqueue q new-state (state-energy new-state)))))))))


(defun adjust-input-for-phase2 (amphipods)
  (let ((extra (list (list #\D (cons 2 *col-amber*))
                     (list #\D (cons 3 *col-amber*))
                     (list #\C (cons 2 *col-bronze*))
                     (list #\B (cons 3 *col-bronze*))
                     (list #\B (cons 2 *col-copper*))
                     (list #\A (cons 3 *col-copper*))
                     (list #\A (cons 2 *col-desert*))
                     (list #\C (cons 3 *col-desert*))))
        (mapped (mapcar (lambda (a)
                          (destructuring-bind (kind (row . col)) a
                            (if (= row 2) (list kind (cons 4 col)) a)))
                        amphipods)))
    (nconc mapped extra)))

(defun make-initial-state (amphipods)
  (make-instance 'search-state
                 :amphipods amphipods
                 :map (build-map amphipods)
                 :energy 0))

(defun part1 (amphipods)
  (find-solution (make-initial-state amphipods)))

(defun part2 (amphipods)
  (let ((*row-room-bottom* 4)
        (extended-amphipods (adjust-input-for-phase2 amphipods)))
    (find-solution (make-initial-state extended-amphipods))))

