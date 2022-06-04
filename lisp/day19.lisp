(defpackage :day19
  (:use :cl :split-sequence :alexandria)
  (:export :parse-input :part1 :part2)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day19)

(defparameter *all-axes*
  (list :positive-x
        :negative-x
        :positive-y
        :negative-y
        :positive-z
        :negative-z))

(defun parse-point (s)
  (->> s
    (str:split #\,)
    (mapcar #'parse-integer)))

(defun parse-group (lines)
  ;; The first line in each group is the scanner number
  (mapcar #'parse-point (cdr lines)))

(defun parse-input (lines)
  (->> lines
    (split-sequence-if #'str:empty?)
    (mapcar #'parse-group)))

(defparameter *identity-mapping*
  '((:positive-x 0)
    (:positive-y 0)
    (:positive-z 0)))

(defun point->scalar (p axis)
  (destructuring-bind (x y z) p
    (case axis
      (:positive-x x)
      (:negative-x (- x))
      (:positive-y y)
      (:negative-y (- y))
      (:positive-z z)
      (:negative-z (- z)))))

(defun sorted-scalars (points axis)
  (let ((scalars (mapcar #'(lambda (p) (point->scalar p axis)) points)))
    (sort scalars #'<)))

(defun apply-mapping (p mapping)
  (destructuring-bind ((x-axis dx) (y-axis dy) (z-axis dz)) mapping
    (list (+ (point->scalar p x-axis) dx)
          (+ (point->scalar p y-axis) dy)
          (+ (point->scalar p z-axis) dz))))

(defun apply-mappings (p mappings)
  (reduce #'apply-mapping mappings :initial-value p))

(defun match-scalars (a b offset n)
  "Given two sorted lists of scalars, return true if we can
   find at least n matches, after applying the given offset to list b."
  (loop while (and a b (plusp n)) do
    (let ((diff (- (car a) (+ (car b) offset))))
      (cond ((minusp diff) (setf a (cdr a)))
            ((plusp diff) (setf b (cdr b)))
            (t (decf n)
               (setf a (cdr a))
               (setf b (cdr b))))))
  (zerop n))

(defun attempt-match (a b axis-a)
  "Attempt to match scanner a on axis_b against scanner b.
   If successful, returns the axis and offset for scanner b."
  (let ((list-a (sorted-scalars a axis-a)))
    (loop for axis-b in *all-axes* do
      (let ((list-b (sorted-scalars b axis-b)))
        (loop for offset from -2000 upto 2000 do
          (when (match-scalars list-a list-b offset 12)
            (return-from attempt-match (list axis-b offset))))))))

(defun match-scanners (a b)
  "Attempt to match two scanners on all axes. If successful, returns
   mappings for the x, y, and z axes."
  (when-let* ((x-map (attempt-match a b :positive-x))              
              (y-map (attempt-match a b :positive-y))
              (z-map (attempt-match a b :positive-z)))
    (list x-map y-map z-map)))

(defun resolve-scanner (resolved s)
  "Attempt to resolve a single scanner against a list of resolved scanners.
   If successful, returns a mapping list."
  (loop for (r mappings) in resolved do
    (when-let (m (match-scanners r s))
      (return (cons m mappings)))))

(defun resolve-scanners (resolved unresolved)
  "Attempt to resolve a list of unresolved scanners against a list
   of resolved scanners. Returns a tuple of newly resolved scanners,
   and still unresolved scanners."
  (let ((newly-resolved nil) (still-unresolved nil))
    (loop for s in unresolved do
      (if-let ((mappings (resolve-scanner resolved s)))
        (push (list s mappings) newly-resolved)
        (push s still-unresolved)))
    (list newly-resolved still-unresolved)))

(defun resolve-all-scanners (scanners)
  "Main resolve loop. We keep track of three lists of scanners. Ones
   that are fully resolved, ones that were resolved in the last iteration,
   and unresolved ones. Each iteration, we try to resolve the unresolved
   scanners against the newly resolved ones."
  (let ((newly-resolved (list (list (first scanners) (list *identity-mapping*))))
        (unresolved (rest scanners))
        (resolved nil))
    (loop while unresolved do
      (destructuring-bind (succeeded failed) (resolve-scanners newly-resolved unresolved)
        (when (not succeeded)
          (error "Failed to resolve scanners"))
        (setf resolved (nconc resolved newly-resolved))
        (setf newly-resolved succeeded)
        (setf unresolved failed)))
    (nconc resolved newly-resolved)))

(defun map-scanner-points (scanner)
  "Take a resolved scanner and return all points for this scanner
   with mappings applied."
  (destructuring-bind (points mappings) scanner
    (mapcar #'(lambda (p) (apply-mappings p mappings)) points)))

(defun get-scanner-position (scanner)
  (destructuring-bind (points mappings) scanner
    (apply-mappings '(0 0 0) mappings)))

(defun manhattan-distance (a b)
  (destructuring-bind (ax ay az) a
    (destructuring-bind (bx by bz) b
      (+ (abs (- ax bx))
         (abs (- ay by))
         (abs (- az bz))))))

(defun max-manhattan-distance (points)
  (loop for a in points
        maximizing
        (loop for b in points
              maximizing
              (manhattan-distance a b))))

(defun part1 (input)
  (-<>> (resolve-all-scanners input)
    (mapcar #'map-scanner-points)
    (apply #'concatenate 'list)
    (remove-duplicates <> :test #'equal)
    (length)))

(defun part2 (input)
  (->> (resolve-all-scanners input)
    (mapcar #'get-scanner-position)
    (max-manhattan-distance)))
