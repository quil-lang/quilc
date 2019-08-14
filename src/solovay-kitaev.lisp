;;;; solovay-kitaev.lisp
;;;;
;;;; Authors: Andrew Shi, Mark Skilbeck

(in-package :cl-quil)

;;; This file contains an implementation of the Solovay-Kitaev
;;; algorithm described in https://arxiv.org/pdf/quant-ph/0505030.pdf,
;;; used to approximately decompose arbitrary unitaries using a finite
;;; set of basis gates.

;;; In the rest of this file, d(x, y) refers to the function of
;;; operator distance defined in the above paper and is equal to ||X -
;;; Y||, the operator norm of (X - Y).
(defparameter +c-gc+ 0.8 "Constant that upper bounds the ratio between d(V, I) [or d(W, I)] and sqrt(d(U, I)) if V and W are balanced group commutators of U [found on page 8 of the paper]. Usually obtained numerically through testing gc-decompose on random unitaries.")

;;; C-approx should theoretically be approximately equal to (* 8 +c-gc+),
;;; but as the paper's authors have shown, a c-approx as low as 2.67
;;; can work in practice [yielding a base approximation distance of
;;; 1/c-approx^2 = 0.14]. We'll use that for now.
(defparameter +c-approx+ 2.67 "Constant that upper bounds the ratio between d(VWV'W', approximate VWV'W') and eps^(3/2), for the balanced group commutators V, W of a unitary and the eps-approximation of each commutator [eps = d(V, approximate V) = d(W, approximate W)]. It is also assumed that d(V, I) = d(W, I) < c-gc * sqrt(eps), which is true for group commutator decompositions made inside the SK-algorithm.")

;;; One-qubit identity and pauli spin matrices, may be replaced later
(defparameter +I+ (magicl:make-complex-matrix 2 2 '(1 0 0 1)))
(defparameter +PX+ (magicl:make-complex-matrix 2 2 '(0 1 1 0)))
(defparameter +PY+ (magicl:make-complex-matrix 2 2 '(0 #C(0 1) #C(0 -1) 0)))
(defparameter +PZ+ (magicl:make-complex-matrix 2 2 '(1 0 0 -1)))
;;; Clifford + T
(defparameter +H+ (magicl:make-complex-matrix 2 2 (let ((a (/ (sqrt 2))))
                                                    (list a a a (- a)))))
(defparameter +S+ (magicl:make-complex-matrix 2 2 '(1 0 0 #C(0 1))))
(defparameter +S-inv+ (magicl:make-complex-matrix 2 2 '(1 0 0 #C(0 -1))))
(defparameter +T+ (magicl:make-complex-matrix 2 2 `(1 0 0 ,(cis (/ pi 4)))))
(defparameter +T-inv+ (magicl:make-complex-matrix 2 2 `(1 0 0 ,(cis (- (/ pi 4))))))
;;; Temporary constant
(defparameter +ballie-radius+ 0.1)

;;; ------------------------------------------------------------------
;;; --------------Various utility functions/structures----------------
;;; ------------------------------------------------------------------

(defun vector-dot-product (a b)
  "Dot product between vectors A and B."
  (assert (= (length a) (length b)))
  (reduce #'+ (loop :for ai :across a :for bi :across b
                    :collect (* ai bi))))

(defun vector-cross-product (a b)
  "Cross product between vectors A and B."
  (assert (= (length a) (length b) 3))
  (let ((ax (aref a 0))
        (ay (aref a 1))
        (az (aref a 2))
        (bx (aref b 0))
        (by (aref b 1))
        (bz (aref b 2))
        (result (make-array 3)))
    (setf (aref result 0) (- (* ay bz) (* az by)))
    (setf (aref result 1) (- (* az bx) (* ax bz)))
    (setf (aref result 2) (- (* ax by) (* ay bx)))
    result))

(defun vector-norm (a)
  "Norm of the vector A."
  (sqrt (vector-dot-product a a)))

(defun vector-distance (a b)
  "Norm of the vector A - B."
  (assert (= (length a) (length b)))
  (sqrt (reduce #'+ (loop :for ai :across a :for bi :across b
                          :collect (expt (- ai bi) 2)))))

(defun vector-normalize (v)
  "Normalizes the vector V in-place; returns its norm."
  (let ((norm (vector-norm v)))
    (assert (> norm 0) nil "ERROR: cannot normalize the zero vector ~A" v)
    (loop :for i :below (length v) :do (setf (aref v i) (/ (aref v i) norm)))
    norm))

;; (defun gate-from-index (gates idx)
;;   "Given an index IDX in parity-inverse convention, return the corresponding basis gate from BASIS-GATES, which is the basis gate indexed by (floor idx 2) and inverted if idx is odd (just the original gate if idx is even)."
;;   (aref gates (floor idx)))

(defstruct commutator
  (v '() :type (or list commutator))
  (w '() :type (or list commutator)))

(defun seq-dagger (op-seq)
  (reverse (mapcar (lambda (x) (logxor #b1 x)) op-seq)))

(defstruct (bloch-vector (:constructor make-bloch-vector))
  "A bloch-vector representation of unitaries as a rotation about an axis on the Bloch sphere."
  (theta 0.0d0 :type double-float)
  (axis #(0 0 0) :type (simple-vector 3)))

(defun random-bloch-vector (max-theta &key (def-theta 0))
  "Generate a random bloch-vector with a maximum rotation angle of MAX-THETA. However, if DEF-THETA is set to a non-zero value, generate a random bloch-vector with the exact rotation angle DEF-THETA."
  (let ((bv (matrix-to-bloch-vector (magicl:random-unitary 2))))
    (setf (bloch-vector-theta bv) (if (zerop def-theta) (random max-theta) def-theta))
    bv))

(defun matrix-trace (m)
  (loop :for i :below (magicl:matrix-cols m)
        :sum (magicl:ref m i i)))

(defun fidelity (m)
  (let ((p (* 2 (log (magicl:matrix-cols m) 2))))
    (/ (+ (expt (abs (matrix-trace m)) 2) p)
       (+ (expt p 2) p))))

;;; Charles
(defun charles-distance (u s)
  (- 1 (fidelity (m* (magicl:conjugate-transpose s) u))))

;;; Not sure which distance measure to use, this one or trace norm or charles' fidelity
(defun operator-dist (u s)
  "Returns d(u, s) = ||U - S||, the operator norm of U - S defined in the paper."
  (let ((sigma (nth-value 1 (magicl:svd (magicl:sub-matrix (bloch-phase-corrected-mat u) (bloch-phase-corrected-mat s))))))
    (loop :for i :below (magicl:matrix-rows sigma) :maximize (magicl:ref sigma i i))))

(defun trace-dist (u s)
  "Returns the trace norm of U - S, equal to Tr[sqrt((U - S)'(U - S))]."
  (let ((sigma (nth-value 1 (magicl:svd (m* (magicl:dagger (magicl:sub-matrix (bloch-phase-corrected-mat u) (bloch-phase-corrected-mat s)))
                                            (magicl:sub-matrix (bloch-phase-corrected-mat u) (bloch-phase-corrected-mat s)))))))
    (loop :for i :below (magicl:matrix-rows sigma) :summing (sqrt (magicl:ref sigma i i)) :into norm :finally (return (/ norm 2)))))

(defun find-c-gc (num-trials &key (distance-function #'operator-dist) (decompose-func #'gc-decompose))
  "Numerically tests for the value of c-gc, the upper bound on the ratio between d(V, I) [or d(W, I)] and sqrt(d(U, I)) if V and W are balanced group commutators of U."
  (loop :for i :below num-trials :for u := (magicl:random-unitary 2) :for v := (funcall decompose-func u) :maximize (/ (funcall distance-function v +I+) (sqrt (funcall distance-function u +I+)))))

;;; ------------------------------------------------------------------
;;; -------------------------THE MEATY PART---------------------------
;;; ------------------------------------------------------------------

(defclass decomposer ()
  ((gates :reader gates
          :initarg :gates
          :type (or cons (vector simple-gate *))
          :documentation "Set of basis gates/operators to decompose to, including inverses.")
   (gate-orders :reader gate-orders
                :initarg :gate-orders
                :type (or cons (vector fixnum *))
                :documentation "Numbers which describe the orders of each basis gate/inverse pair.")
   (num-qubits :reader num-qubits
               :initarg :num-qubits
               :type non-negative-fixnum
               :documentation "Number of qubits the operators of this decomposer act on.")
   (epsilon0 :reader epsilon0
             :initarg :epsilon0
             :type double-float
             :documentation "Parameter controlling the density of base-approximation unitaries for this decomposer. Specifically, the angle-axis ball for NUM-QUBITS will be divided into segments of SUBDIVISION along each axis.")
   (subdivision :reader subdivision
                :initarg :subdivision
                :type double-float
                :documentation "Parameter controlling the density of base-approximation unitaries for this decomposer. Specifically, the angle-axis ball for NUM-QUBITS will be divided into segments of SUBDIVISION along each axis.")
   (base-approximations :reader base-approximations
                        :initarg :base-approximations
                        :type hash-table
                        :documentation "A set of base approximations such that every unitary operator on NUM-QUBITS (all operators in SU(2^NUM-QUBITS)) is within EPSILON0 of some unitary in the set."))
  (:default-initargs
   :gates (error ":GATES is a required initarg to DECOMPOSER.")
   :gate-orders '()
   :num-qubits (error ":NUM-QUBITS is a required initarg to DECOMPOSER.")
   :epsilon0 1)
  (:documentation "A decomposer which uses the Solovay-Kitaev algorithm to approximately decompose arbitrary unitaries to a finite set of basis gates."))

(defun make-decomposer (basis-gates num-qubits epsilon0)
  "Initializer for a unitary decomposer."
  ;; (assert (< epsilon0 (/ (expt +c-approx+ 2))) (epsilon0) "ERROR: the provided base approximation epsilon ~A is not less than ~A, which it must be for approximations to improve on each iteration." epsilon0 (/ (expt +c-approx+ 2)))
  (make-instance 'decomposer
                 :gates (loop :for gate :in basis-gates
                              :collect gate #+ignore(simple-gate-matrix gate)
                              :collect (magicl:dagger gate #+ignore(simple-gate-matrix gate)))
                 :gate-orders (loop :for gate :in basis-gates :collect (find-order gate))
                 :num-qubits num-qubits
                 :epsilon0 epsilon0
                 :subdivision +ballie-radius+ ;; TODO
                 :base-approximations (generate-base-approximations-ballies basis-gates :depth-limit 23 :verbose t)))

;;; NOTE: REQUIRES 1e-6 TOLERANCE ON MAGICL:IDENTITYP
(defun find-order (mat)
  "Determines the order of MAT, i.e. how many times it has to be multiplied with itself to reach the identity matrix."
  (let ((curr-mat mat))
    (loop :for order :from 2 :to 50
          :do (setf curr-mat (m* mat curr-mat))
          :when (magicl:identityp curr-mat)
            :do (return order)
          :finally (return 0))))

;;; --------------------------------------------------------------
;;; --------------BASE APPROXIMATION GENERATION-------------------
;;; --------------------------------------------------------------

;;; The general procedure here is to use what I call the "angle-axis
;;; ball" mapping of SU(2) (technically PU(2), the group of SU(2)
;;; modulo global phase). A unitary which is a rotation about an axis
;;; in 3d space by an angle theta is mapped to a point of radius theta
;;; in the direction of the rotation axis; thus, the entire group is
;;; mapped to a ball of radius pi. The Euclidean distance between
;;; points in the angle-axis ball is not perfectly correlated with our
;;; desired metric of operator distance, but is related closely enough
;;; to serve as a surprisingly good heuristic. It is also much more
;;; convenient to work with than a 3-sphere, which is a big reason for
;;; why I'm using this.

(defun bloch-vector-to-ball-coord (bv)
  "Returns the axis-angle ball coordinate of a bloch-vector BV."
  (concatenate 'vector (loop :for x :across (bloch-vector-axis bv)
                             :collect (* x (bloch-vector-theta bv)))))

(defun aa-ball-distance (bv1 bv2)
  "Distance on the axis-angle ball between bloch-vectors BV1 and BV2."
  (vector-distance (bloch-vector-to-ball-coord bv1) (bloch-vector-to-ball-coord bv2)))

(defun epsilon0-from-ball-division (num-trials subdivision)
  "Numerically computes the max value of epsilon0 that a grid of spacing SUBDIVISION on the angle-axis ball would satisfy."
  (loop :for i :below num-trials
        :for bv1 := (random-bloch-vector (/ pi 2))
        :for bv2 := (random-bloch-vector (/ pi 2))
        :for bv-dist := (aa-ball-distance bv1 bv2)
        :for mat-dist := (operator-dist (bloch-vector-to-matrix bv1) (bloch-vector-to-matrix bv2))
        ;; Checks if the random unitaries picked have an angle-axis
        ;; ball distance shorter than the diagonal of a grid cube
        :when (< bv-dist (sqrt (* 3 (expt subdivision 2))))
          :maximize mat-dist :into max-dist :and :sum mat-dist :into total :and :sum 1 :into num
        :finally (return (list max-dist (/ total num)))))

(defun epsilon0-vs-ball-dist (num-trials dist)
  "Numerically computes the max value of epsilon0 that DIST on the angle-axis ball would satisfy."
  (loop :for i :below num-trials
        :for bv1 := (random-bloch-vector (/ pi 2))
        :for bv2 := (random-bloch-vector (/ pi 2))
        :for bv-dist := (aa-ball-distance bv1 bv2)
        :for mat-dist := (operator-dist (bloch-vector-to-matrix bv1) (bloch-vector-to-matrix bv2))
        ;; Checks if the random unitaries picked have an angle-axis
        ;; ball distance shorter than the diagonal of a grid cube
        :when (< bv-dist dist)
          :maximize mat-dist :into max-dist :and :sum mat-dist :into total :and :sum 1 :into num
        :finally (return (list max-dist (/ total num)))))

(defun matrix-to-grid-coord (mat subdivision)
  "Returns the grid coordinate of MAT when mapped to an axis-angle ball coordinate in a grid of size SUBDIVISION. The result is an array of 3 decimals corresponding to MAT's spatial coordinate in the grid."
  (concatenate 'vector (loop :for x :across (bloch-vector-to-ball-coord (matrix-to-bloch-vector mat))
                             :collect (/ x subdivision))))

;; (defun num-reachable-areas (subdivision)
;;   "Returns the total number of grid cubes of length SUBDIVISION that are reachable within the angle-axis ball, i.e. the number of cubes which intersect the ball."
;;   (let ((diameter (1+ (floor (/ (* 2 pi) subdivision))))
;;         (offset (ceiling (/ pi subdivision))))
;;     (dotimes (i (expt diameter 3))
;;       (let ((x (- (mod i diameter) offset))
;;             (y (- (mod (floor i diameter) diameter) offset))
;;             (z (- (floor i (expt diameter 2)) offset)))))))

;;; Generate a hash table of grid coordinate -> unitary. Populate
;;; using IDDFS, as we want to minimize approximation lengths.
(defun generate-base-approximations-ballies (basis-gates &key (ballie-r +ballie-radius+) (depth-limit 10) (verbose nil) (debug nil)) ;; TODO
  "Generates a set of base approximations such that every unitary operator on NUM-QUBITS (all operators in SU(2^NUM-QUBITS)) is within EPSILON0 of some unitary in the set. The approximations are returned as a hash map from each grid block in the axis-angle ball to the unitary that approximates that block."
  ;; Gates of odd index [2n + 1] correspond to the inverse of gate [2n]
  (let* ((gates (loop :for gate :in basis-gates
                      :collect gate #+ignore(simple-gate-matrix gate)
                      :collect (magicl:dagger gate #+ignore(simple-gate-matrix gate)))) ;; TODO
         (gate-orders (loop :for gate :in basis-gates :collect (find-order gate)))
         (approx-table (make-hash-table :test 'equalp))
         (max-depth 0)
         (prev-count 0)
         (overcounted 0)
         (neglected 0)
         (oeis-magic-num 12345678))
    (loop :for gate :in gates
          :for i :from 0
          :do (format verbose "Gate ~D: ~D (order = ~D)~%" i gate (elt gate-orders (floor i 2))))
    (labels ((helper (depth seq mat last-idx rep-count)
               (cond ((= depth max-depth)
                      (let* ((exact-grid-coord (matrix-to-grid-coord mat ballie-r))
                             (valid-ballies (remove-if (lambda (x) (or (> (vector-norm x) (/ pi ballie-r))
                                                                       (nth-value 1 (gethash x approx-table))
                                                                       (> (vector-distance x exact-grid-coord) 1)))
                                                       (loop :for i :below 8 :collect (vector
                                                                                       (+ (floor (aref exact-grid-coord 0)) (mod i 2))
                                                                                       (+ (floor (aref exact-grid-coord 1)) (mod (floor i 2) 2))
                                                                                       (+ (floor (aref exact-grid-coord 2)) (floor i 4)))))))
                        ;; Find the closest ball out of neighboring, empty ballies (within the aa-ball)
                        (dolist (ballie valid-ballies)
                          (setf (gethash ballie approx-table) seq))
                        (when (not valid-ballies)
                          (format debug "~D overcounted~%" seq)
                          (incf overcounted))))
                     (t (loop :for gate :in gates
                              :for i :from 0
                              :for order := (elt gate-orders (floor i 2))
                              ;; Don't use a gate if a) it has an odd
                              ;; index and its inverse is already in
                              ;; the gate set, b) we're repeating the
                              ;; previous gate and its half-order is
                              ;; exceeded, or c) we're using the
                              ;; previous gate's inverse
                              :if (and (not (and (= order 2) (oddp i)))
                                       (or (and (= i last-idx)
                                                (or (zerop order) (< rep-count
                                                                     (- (floor order 2) (if (and (evenp order) (oddp i)) 1 0)))))
                                           (and (not (= i last-idx))
                                                (not (= (floor i 2) (floor last-idx 2))))))
                                :do (helper (1+ depth) (cons i seq) (m* mat gate) i (if (= i last-idx) (1+ rep-count) 1))
                              :else ;; Look at which sequences are being pruned
                              :do (format debug "Nonono! Not using gate ~A on sequence ~A~%" i seq)
                                  (incf neglected))))))
      (format verbose "Division: ~D~%" ballie-r)
      (format (not verbose) "Generating base approximations...")
      (loop :for curr-depth :from 0 :to depth-limit :do
        (format verbose "Depth ~D search:" curr-depth)
        (finish-output)
        (setf max-depth curr-depth)
        (setf overcounted 0)
        (setf neglected 0)
        (helper 0 '() +I+ -1 0)
        (format verbose " (~D new, ~D overcounted, ~D neglected)" (- (hash-table-count approx-table) prev-count) overcounted neglected)
        (format verbose" (~$% filled so far, ~D to go)~%" (* 100 (/ (hash-table-count approx-table) oeis-magic-num))
                (- oeis-magic-num (hash-table-count approx-table)))
        (format (not verbose) ".")
        (setf prev-count (hash-table-count approx-table)))
      approx-table)))

;;; Will just be a hash table lookup
(defun fetch-base-approximation (base-approximations u) ;; TODO
  "Returns the base case approximation for a unitary U, represented as a list of indices in parity-inverse convention."
  (let* ((exact-grid-coord (matrix-to-grid-coord u +ballie-radius+))
         (nearby-ballies (loop :for i :below 8 :collect (vector
                                                         (+ (floor (aref exact-grid-coord 0)) (mod i 2))
                                                         (+ (floor (aref exact-grid-coord 1)) (mod (floor i 2) 2))
                                                         (+ (floor (aref exact-grid-coord 2)) (floor i 4))))))
    (gethash (reduce (lambda (x y) (if (< (vector-distance x exact-grid-coord) (vector-distance y exact-grid-coord))
                                       x y))
                     (cdr nearby-ballies) :initial-value (car nearby-ballies))
             base-approximations)))

;;; ---------------------------------------------------------------
;;; ---------------------Algorithm Calling-------------------------
;;; ---------------------------------------------------------------

(defun brute-base (u decomposer max-depth &key (epsilon0 0.14))
  "Testing brute force base approximation search."
  (labels ((helper (depth seq mat last-idx rep-count)
             (cond ((= depth max-depth)
                    (if (< (operator-dist u mat) epsilon0)
                        seq
                        nil))
                   (t (loop :for gate :in (gates decomposer)
                            :for i :from 0
                            :for order := (elt (gate-orders decomposer) (floor i 2))
                            ;; Don't use a gate if a) it has an odd
                            ;; index and its inverse is already in
                            ;; the gate set, b) we're repeating the
                            ;; previous gate and its half-order is
                            ;; exceeded, or c) we're using the
                            ;; previous gate's inverse
                            :if (and (not (and (= order 2) (oddp i)))
                                     (or (and (= i last-idx)
                                              (or (zerop order) (< rep-count
                                                                   (- (floor order 2) (if (and (evenp order) (oddp i)) 1 0)))))
                                         (and (not (= i last-idx))
                                              (not (= (floor i 2) (floor last-idx 2))))))
                              :do (let ((found-seq (helper (1+ depth) (cons i seq) (m* mat gate) i (if (= i last-idx) (1+ rep-count) 1))))
                                    (when found-seq (return found-seq))))))))
    (let ((discovered-seq (helper 0 '() +I+ -1 0)))
      (if discovered-seq
          discovered-seq
          (format t "~%Nothing found for unitary ~A..." u)))))

;;; Helper method for iterating through SK
(defun sk-iter (decomposer u n)
  "An approximation iteration within the Solovay-Kitaev algorithm at a depth N. Returns a vector of two items, which are each either a commutator or a base approximation."
  (if (zerop n)
      (fetch-base-approximation (base-approximations decomposer) u) #+ignore(gethash (map 'vector #'floor (matrix-to-grid-coord u +ballie-radius+)) base-approximations)
      (let ((next-u (sk-iter decomposer u (1- n))))
        (multiple-value-bind (v w) (gc-decompose (m* u (magicl:dagger (decompress-and-multiply decomposer next-u))))
          (let* ((v-next (sk-iter decomposer v (1- n)))
                 (w-next (sk-iter decomposer w (1- n))))
            (cons next-u (list (make-commutator :v v-next :w w-next))))))))

(defun brute-sk-iter (decomposer u n &key (epsilon0 0.14))
  "Brute force iteration"
  (if (zerop n)
      (brute-base u decomposer 18 :epsilon0 epsilon0)
      (let ((next-u (brute-sk-iter decomposer u (1- n) :epsilon0 epsilon0)))
        (multiple-value-bind (v w) (gc-decompose (m* u (magicl:dagger (decompress-and-multiply decomposer next-u))))
          (let* ((v-next (brute-sk-iter decomposer v (1- n) :epsilon0 epsilon0))
                 (w-next (brute-sk-iter decomposer w (1- n) :epsilon0 epsilon0)))
            (cons next-u (list (make-commutator :v v-next :w w-next))))))))

(defun decompose (decomposer unitary &key (depth 3) (epsilon0 0.14))
  "Decomposes a unitary into a list of commutator objects terminated by a base approximation to U. When expanded into all its constituent unitaries, this decomposition is guaranteed to be within EPSILON of the original unitary."
  ;; (ceiling (log (/ (log (* epsilon +c-approx+ +c-approx+))
  ;;                                (log (* eps0 +c-approx+ +c-approx+))))
  ;;                        (log (/ 3 2)))
  (let* ((eps0 (epsilon0 decomposer)))
    (brute-sk-iter decomposer unitary depth :epsilon0 epsilon0) #+ignore(sk-iter decomposer unitary depth)))

(defun decompress (decomposer item)
  "Expands the commutators in ITEM (which can be a commutator or a sequence of commutators and fixnums) and retrieves the appropriate gate for each parity-inverse index inside, returning the decompressed list of gates."
  ;; TODO: still need to convert indices -> gates
  (if (typep item 'commutator)
      (append (seq-dagger (decompress decomposer (commutator-w item)))
              (seq-dagger (decompress decomposer (commutator-v item)))
              (decompress decomposer (commutator-w item))
              (decompress decomposer (commutator-v item)))
      (if (typep (car item) 'list)
          (append (decompress decomposer (car item)) (decompress decomposer (cadr item)))
          item)))

(defun decompress-and-multiply (decomposer item)
  "Returns the matrix produced by expanding and multiplying together all the gates represented inside ITEM."
  (if (typep item 'commutator)
      (m* (decompress-and-multiply decomposer (commutator-v item))
          (decompress-and-multiply decomposer (commutator-w item))
          (magicl:dagger (decompress-and-multiply decomposer (commutator-v item)))
          (magicl:dagger (decompress-and-multiply decomposer (commutator-w item))))
      (if (typep (car item) 'list)
          (m* (decompress-and-multiply decomposer (cadr item)) (decompress-and-multiply decomposer (car item)))
          (reduce #'m*
                  (reverse (mapcar (lambda (x) (elt (gates decomposer) x)) item))
                  :initial-value +I+))))

(defun approximate (decomposer u acc)
  "An alternative function for decomposition that approximates a unitary U to an arbitrary accuracy ACC (using decomposer)."
  (let* ((seq (brute-base u decomposer 18))
         (curr-approx (decompress-and-multiply decomposer seq))
         (curr-depth 0))
    (loop :while (< acc (operator-dist u curr-approx))
          :do (uiop:nest
               (multiple-value-bind (v w) (gc-decompose (m* u (magicl:dagger curr-approx))))
               #+ignore(multiple-value-bind (v-approx v-approx-mat) (approximate decomposer v (* +c-approx+ (expt curr-acc 3/2))))
               #+ignore(multiple-value-bind (w-approx w-approx-mat) (approximate decomposer w (* +c-approx+ (expt curr-acc 3/2))))
               (let* ((v-approx (decompose decomposer v :depth curr-depth))
                      (v-approx-mat (decompress-and-multiply decomposer v-approx))
                      (w-approx (decompose decomposer w :depth curr-depth))
                      (w-approx-mat (decompress-and-multiply decomposer w-approx))))
               (progn (setf curr-approx (m* v-approx-mat w-approx-mat (magicl:dagger v-approx-mat) (magicl:dagger w-approx-mat) curr-approx))
                      (setf seq (cons seq (list (make-commutator :v v-approx :w w-approx))))
                      (incf curr-depth)))
          :finally (return (values seq curr-approx)))))

;;; Conversions between matrix and bloch-vector representations of
;;; unitaries. To understand them, remember/note that for a rotation
;;; of an angle theta about the bloch sphere axis <x, y, z> with unit
;;; norm, the corresponding matrix representation is U = cos(t/2)*I -
;;; isin(t/2) * (x*X + y*Y + z*Z), where t = theta/2 and X, Y, Z are
;;; the usual Pauli matrices (I = identity). Thus, a unitary obtained
;;; from this representation would have the form
;;;
;;;           /                                           \
;;;           | cos(t) - z*i*sin(t)    -sin(t)*(x*i + y)  |
;;;           |                                           |
;;;           |  -sin(t)*(x*i - y)    cos(t) + z*i*sin(t) |
;;;           \                                           /
;;;
;;; up to a global phase factor. Using the equation, we can convert
;;; from bloch-vector to matrix, and using this matrix, we can extract
;;; the bloch-vector parameters to convert back, which is what the
;;; functions below do.
(defun matrix-to-bloch-vector (mat)
  "Returns the bloch-vector corresponding to the unitary matrix MAT."
  (let* ((phased-mat (bloch-phase-corrected-mat mat))
         (x-sin (* -1 (imagpart (magicl:ref phased-mat 0 1))))
         (y-sin (realpart (magicl:ref phased-mat 1 0)))
         (z-sin (imagpart (/ (- (magicl:ref phased-mat 1 1) (magicl:ref phased-mat 0 0)) 2)))
         (cos-theta (realpart (/ (+ (magicl:ref phased-mat 0 0) (magicl:ref phased-mat 1 1)) 2)))
         (sin-theta (sqrt (+ (expt x-sin 2) (expt y-sin 2) (expt z-sin 2))))
         (theta (* 2 (atan sin-theta cos-theta)))
         (axis (make-array 3)))
    (setf (aref axis 0) (if (zerop sin-theta) 0 (/ x-sin sin-theta)))
    (setf (aref axis 1) (if (zerop sin-theta) 0 (/ y-sin sin-theta)))
    (setf (aref axis 2) (if (zerop sin-theta) 0 (/ z-sin sin-theta)))
    (make-bloch-vector :theta theta :axis axis)))

(defun bloch-vector-to-matrix (bv)
  "Returns the unitary matrix corresponding to the bloch-vector BV."
  (let* ((half-theta (/ (bloch-vector-theta bv) 2))
         (axis (bloch-vector-axis bv))
         (nx (aref axis 0))
         (ny (aref axis 1))
         (nz (aref axis 2))
         (axis-factor (* #C(0 -1) (sin half-theta))))
    (magicl:add-matrix (magicl:scale (cos half-theta) +I+)
                       (magicl:scale (* nx axis-factor) +PX+)
                       (magicl:scale (* ny axis-factor) +PY+)
                       (magicl:scale (* nz axis-factor) +PZ+))))

(defun bloch-phase-corrected-mat (mat)
  "Returns a matrix equal to MAT with corrected global phase such that it is in bloch-vector matrix form described in the comment above."
  (let* ((diag-sum (+ (magicl:ref mat 0 0) (magicl:ref mat 1 1)))
         (off-diag-sum (+ (magicl:ref mat 1 0) (magicl:ref mat 0 1)))
         (off-diag-diff (- (magicl:ref mat 1 0) (magicl:ref mat 0 1)))
         (diag-diff (- (magicl:ref mat 0 0) (magicl:ref mat 1 1)))
         (phase-nums (list diag-sum off-diag-sum off-diag-diff diag-diff)))
    ;; In a matrix directly obtained from expanding the bloch-vector
    ;; representation, the sum of the diagonal and the difference of
    ;; the off-diagonal should be purely real. Likewise, the diagonal
    ;; difference and the off-diagonal sum should be purely
    ;; imaginary. Thus, we use the first non-zero number in these
    ;; quantities to find our phase correction.
    (magicl:scale (loop :for i :below 4
                        :for num :in phase-nums
                        :when (not (zerop num))
                          :do (return (* (/ (abs num) num) (if (evenp i) 1 #C(0 -1)))))
                  mat)))

;;; ------------------------------------------------------------------
;;; -------------FUNCTIONS FOR FINDING GROUP COMMUTATORS--------------
;;; ------------------------------------------------------------------
;;; Overall procedure taken in https://github.com/cmdawson/sk to find
;;; balanced group commutators V and W for a unitary U (the ' symbol
;;; represents a dagger):
;;;
;;;    1) Convert U to its bloch-vector representation, which is a
;;;       rotation by some theta around an arbitrary axis.
;;;
;;;    2) Find unitaries S and Rx s.t. Rx is a rotation around the X
;;;       axis by theta and SRxS' = U.
;;;
;;;    3) Find the group commutators B, C for Rx s.t. Rx = BCB'C'.
;;;
;;; With A, B, and S, we can set V = SBS' and W = SCS', because then
;;; VWV'W' = SBS'SCS'SB'S'SC'S' = SBCB'C'S' = SRxS' = U.

(defun unitary-to-conjugated-x-rotation (u)
  "Given a unitary U, returns unitaries S and Rx such that Rx is a rotation around the X axis by the same angle that U rotates around its axis, and U = SRxS'."
  (let* ((u-bv (matrix-to-bloch-vector u))
         (rx-bv (make-bloch-vector :theta (bloch-vector-theta u-bv) :axis #(1 0 0)))
         (rx (bloch-vector-to-matrix rx-bv)))
    (values (find-transformation-matrix u rx) rx)))

(defun find-transformation-matrix (a b)
  "Given unitaries A and B, finds the unitary S such that A = SBS'."
  ;; The cross product of a-axis and b-axis represents an axis
  ;; orthogonal to the axes of rotation for a and b. Thus, a
  ;; rotation around this orthogonal axis by the angle between
  ;; a-axis and b-axis is a transformation which does what we want.
  (let* ((a-bv (matrix-to-bloch-vector a))
         (b-bv (matrix-to-bloch-vector b))
         (a-axis (bloch-vector-axis a-bv))
         (b-axis (bloch-vector-axis b-bv))
         (dot-prod (vector-dot-product a-axis b-axis))
         (cross-prod (vector-cross-product b-axis a-axis))
         (result-bv (make-bloch-vector)))
    ;; Only bother finding an axis if the vectors aren't parallel
    (unless (and (zerop (vector-norm cross-prod)) (double~ dot-prod 0))
      (cond ((zerop (vector-norm cross-prod)) nil) ;; very special anti-parallel case
            (t                                     ;; General case
             (vector-normalize cross-prod)
             (setf (bloch-vector-axis result-bv) cross-prod)
             (setf (bloch-vector-theta result-bv) (acos dot-prod)))))
    (bloch-vector-to-matrix result-bv)))

(defun gc-decompose-x-rotation (u)
  "Given a unitary U, returns two values B and C which are the balanced commutators of U (i.e. U = [B, C] = BCB'C'). IMPORTANT: U must be a rotation about the X axis; this is not the general function for any U."
  (let* ((u-cos-half-theta (cos (/ (bloch-vector-theta (matrix-to-bloch-vector u)) 2)))
         (st (expt (/ (- 1 u-cos-half-theta) 2) 1/4))
         (ct (sqrt (- 1 (expt st 2))))
         (theta (* 2 (asin st)))
         (alpha (atan st))
         (b-axis (make-array 3))
         (w-axis (make-array 3)))
    (setf (aref w-axis 0) (* st (cos alpha)))
    (setf (aref b-axis 0) (* st (cos alpha)))
    (setf (aref w-axis 1) (* st (sin alpha)))
    (setf (aref b-axis 1) (* st (sin alpha)))
    (setf (aref w-axis 2) ct)
    (setf (aref b-axis 2) (- ct))
    (let ((b (bloch-vector-to-matrix (make-bloch-vector :theta theta :axis b-axis)))
          (w (bloch-vector-to-matrix (make-bloch-vector :theta theta :axis w-axis))))
      (values b (find-transformation-matrix w (magicl:dagger b))))))

(defun gc-decompose (u)
  "Finds the balanced group commutators V and W for any unitary U, returning a commutator object."
  (let* ((u-theta (bloch-vector-theta (matrix-to-bloch-vector u)))
         (rx-theta (bloch-vector-to-matrix (make-bloch-vector :theta u-theta :axis #(1 0 0))))
         (s (find-transformation-matrix u rx-theta)))
    (multiple-value-bind (b c) (gc-decompose-x-rotation rx-theta)
      (values (m* s b (magicl:dagger s))
              (m* s c (magicl:dagger s))))))

(defun gc-decompose-alt (u)
  "Alternative group commutator decomposition."
  (let* ((u-cos-half-theta (cos (/ (bloch-vector-theta (matrix-to-bloch-vector u)) 2)))
         (phi (* 2 (asin (expt (/ (- 1 u-cos-half-theta) 2) 1/4))))
         (v (bloch-vector-to-matrix (make-bloch-vector :theta phi :axis #(1 0 0))))
         (w (bloch-vector-to-matrix (make-bloch-vector :theta phi :axis #(0 1 0))))
         (s (find-transformation-matrix u (m* v w (magicl:dagger v) (magicl:dagger w)))))
    (values (m* s v (magicl:dagger s)) (m* s w (magicl:dagger s)))))

;;; Function for testing the quality of approximations as a function of the base approximation error
(defun test-base-approximations (decomposer start end &key (depth 3) (step-size 0.1) (trials 100))
  (let ((avg-error 0))
    (loop :for eps0 :from start :to end :by step-size :do
      (setf avg-error 0)
      (dotimes (i trials)
        (let* ((u (magicl:random-unitary 2))
               (u-approx (decompress-and-multiply decomposer (decompose decomposer u :depth depth :epsilon0 eps0))))
          (incf avg-error (operator-dist u u-approx))))
      (format t "~%Average error for eps0 = ~A: ~A" eps0 (/ avg-error trials)))))

;;; Some functions which explore the ball representation of SU(2)
(defun ball-op-distances ()
  (let* ((bv1 (random-bloch-vector pi))
         (bv2 (random-bloch-vector pi))
         (u1 (bloch-vector-to-matrix bv1))
         (u2 (bloch-vector-to-matrix bv2))
         (ball1 (make-array 3))
         (ball2 (make-array 3)))
    (loop :for i :below 3
          :for i1 :across (bloch-vector-axis bv1)
          :for i2 :across (bloch-vector-axis bv2)
          :do (setf (aref ball1 i) (* i1 (bloch-vector-theta bv1)))
              (setf (aref ball2 i) (* i2 (bloch-vector-theta bv2))))
    (values (operator-dist u1 u2) (vector-distance ball1 ball2))))

(defun ball-op-distance-ratios (num-trials)
  (let ((min-ratio MOST-POSITIVE-FIXNUM)
        (max-ratio MOST-NEGATIVE-FIXNUM))
    (dotimes (i num-trials)
      (multiple-value-bind (op-dist ball-dist) (ball-op-distances)
        (setf min-ratio (min min-ratio (/ op-dist ball-dist)))
        (setf max-ratio (max max-ratio (/ op-dist ball-dist)))))
    (format t "~%TESTING RATIO OF OPERATOR DISTANCE TO BALL DISTANCE~%Min: ~A~%Max: ~A~%" min-ratio max-ratio)))

(defun dist-range (num-trials max-angle &key (distance-function #'operator-dist))
  (let ((min-dist MOST-POSITIVE-FIXNUM)
        (max-dist MOST-NEGATIVE-FIXNUM))
    (loop :for i :below num-trials
          :for bv1 := (random-bloch-vector max-angle)
          :for bv2 := (random-bloch-vector max-angle)
          :for u1 := (bloch-vector-to-matrix bv1)
          :for u2 := (bloch-vector-to-matrix bv2)
          :for dist := (funcall distance-function u1 u2)
          :do (setf min-dist (min min-dist dist))
              (setf max-dist (max max-dist dist)))
    (format t "~%[TESTING THE RANGE OF DISTANCES]~%Up to max-angle: ~A~%Min op dist: ~A~%Max op dist: ~A~%" max-angle min-dist max-dist)))

(defun search-op-variations (num-trials target-dist &key (tolerance 0))
  (let ((min-dist MOST-POSITIVE-FIXNUM)
        (max-dist MOST-NEGATIVE-FIXNUM)
        (hits 0))
    (loop :for i :below num-trials
          :for bv1 := (random-bloch-vector (/ pi 2))
          :for bv2 := (random-bloch-vector (/ pi 2))
          :for bv-dist := (aa-ball-distance bv1 bv2)
          :for op-dist := (operator-dist (bloch-vector-to-matrix bv1) (bloch-vector-to-matrix bv2))
          :when (if (zerop tolerance) (< bv-dist target-dist) (< (abs (- bv-dist target-dist)) tolerance))
            :do (setf min-dist (min min-dist op-dist))
                (setf max-dist (max max-dist op-dist))
                (incf hits))
    (format t "~%[TESTING VARIATION OF OPERATOR DISTANCES]~%")
    (format t "Num trials: ~A~%Target ball dist: ~A~%Max op dist: ~A~%Percent hits: ~A~%"
            num-trials target-dist max-dist (/ hits num-trials 1.0))
    (unless (zerop tolerance)
      (format t "(Additional data)~%Tolerance: ~A~%Min op dist: ~A~%% of entire interval [0, 1.2] taken up: ~A~%"
              tolerance min-dist (/ (- max-dist min-dist) 1.2)))))

(defun search-ball-variations (num-trials target-dist &key (tolerance 0))
  (let ((min-dist MOST-POSITIVE-FIXNUM)
        (max-dist MOST-NEGATIVE-FIXNUM)
        (hits 0))
    (loop :for i :below num-trials
          :for bv1 := (random-bloch-vector (/ pi 2))
          :for bv2 := (random-bloch-vector (/ pi 2))
          :for bv-dist := (aa-ball-distance bv1 bv2)
          :for op-dist := (operator-dist (bloch-vector-to-matrix bv1) (bloch-vector-to-matrix bv2))
          :when (if (zerop tolerance) (< op-dist target-dist) (< (abs (- op-dist target-dist)) tolerance))
            :do (setf min-dist (min min-dist bv-dist))
                (setf max-dist (max max-dist bv-dist))
                (incf hits))
    (format t "~%[TESTING VARIATION OF BALL DISTANCES]~%")
    (format t "Num trials: ~A~%Target op dist: ~A~%Max ball dist: ~A~%Percent hits: ~A~%"
            num-trials target-dist max-dist (/ hits num-trials 1.0))
    (unless (zerop tolerance)
      (format t "(Additional data)~%Tolerance: ~A~%Min op dist: ~A~%% of entire interval [0, pi] taken up: ~A~%"
              tolerance min-dist (/ (- max-dist min-dist) pi)))))

(defun compare-variations (num-trials tolerance-var)
  (search-ball-variations num-trials tolerance-var)
  (search-op-variations num-trials (* tolerance-var (/ pi (sqrt 2)))))
