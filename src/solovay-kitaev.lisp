;;;; solovay-kitaev.lisp
;;;;
;;;; Authors: Andrew Shi, Mark Skilbeck

(in-package :cl-quil)

;;; This file contains an implementation of the Solovay-Kitaev
;;; algorithm described in https://arxiv.org/pdf/quant-ph/0505030.pdf,
;;; used to approximately decompose arbitrary unitaries using a finite
;;; set of basis gates.

(defparameter eps 1e-7)
(defparameter +c-approx+ (* 4 (sqrt 2)))
(defparameter +c-gc+ (/ (sqrt 2)))
(defparameter +I+ (magicl:make-complex-matrix 2 2 '(1 0 0 1)))
(defparameter +PX+ (magicl:make-complex-matrix 2 2 '(0 1 1 0)))
(defparameter +PY+ (magicl:make-complex-matrix 2 2 '(0 #C(0 1) #C(0 -1) 0)))
(defparameter +PZ+ (magicl:make-complex-matrix 2 2 '(1 0 0 -1)))

;;; --------------Various utility functions/structures----------------

(defun vector-dot-product (a b)
  "Dot product between vectors A and B."
  (assert (= (length a) (length b)))
  (let ((result 0))
    (loop :for ai :across a :for bi :across b
          :do (incf result (* ai bi)))
    result))

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

(defun vector-norm (v)
  "Norm of the vector V."
  (sqrt (vector-dot-product v v)))

(defun vector-normalize (v)
  "Normalizes the vector V in-place; returns its norm."
  (let ((norm (vector-norm v)))
    (assert (> norm eps) nil "ERROR: cannot normalize the zero vector ~A" v)
    (loop :for i :below (length v) :do (setf (aref v i) (/ (aref v i) norm)))
    norm))

(defun basis-gate-from-index (basis-gates idx)
  "Given an index IDX in sign-inverse convention, return the corresponding basis gate from BASIS-GATES. Specifically, if IDX is negative, return the inverse of the corresponding gate, which is the element in BASIS-GATES indexed by (absolute value of IDX minus 1)."
  (aref basis-gates (1- (abs idx))))

(defstruct commutator
  (v '() :type list)
  (w '() :type list))

(defun multiply-commutator (comm)
  comm)

(defun expand-commutator (comm)
  (let ((v (commutator-v comm))
        (w (commutator-w comm)))
    (append v w (dagger v) (dagger w))))

(defun dagger (op-seq)
  (reverse (mapcar #'(lambda (x) (* -1 x)) op-seq)))

(defun matrix-trace (m)
  (loop :for i :below (magicl:matrix-cols m)
        :sum (magicl:ref m i i)))

(defun fidelity (m)
  (let ((p (* 2 (log (magicl:matrix-cols m) 2))))
    (/ (+ (expt (abs (matrix-trace m)) 2) p)
       (+ (expt p 2) p))))

;;; Charles
(defun distance (u s)
  (- 1 (fidelity (magicl:multiply-complex-matrices
                  (magicl:conjugate-transpose s)
                  u))))

;;; --------------------------THE MEATY PART--------------------------

(defclass decomposer ()
  ((basis-gates :reader basis-gates
                :initarg :basis-gates
                :type (vector simple-gate *)
                :initform (error ":BASIS-GATES is a required initarg to DECOMPOSER.")
                :documentation "Set of basic gates/operators to decompose to.")
   (num-qubits :reader num-qubits
               :initarg :num-qubits
               :type non-negative-fixnum
               :initform (error ":NUM-QUBITS is a required initarg to DECOMPOSER.")
               :documentation "Number of qubits the operators of this decomposer act on.")
   (epsilon0 :reader epsilon0
             :initarg :epsilon0
             :type double-float
             :initform (error ":EPSILON0 is a required initarg to DECOMPOSER.")
             :documentation "Parameter controlling the density of base-approximation unitaries for this decomposer. Specifically, every unitary operator on NUM-QUBITS should be within EPSILON0 of some unitary in BASE-APPROXIMATIONS.")
   (base-approximations :accessor base-approximations
                        :initarg :base-approximations
                        :initform nil
                        :documentation "A set of base approximations such that every unitary operator on NUM-QUBITS (all operators in SU(2^NUM-QUBITS)) is within EPSILON0 of some unitary in the set."))
  (:documentation "A decomposer which uses the Solovay-Kitaev algorithm to approximately decompose arbitrary unitaries to a finite set of basis gates."))

(defun make-decomposer (basis-gates num-qubits epsilon0)
  "Initializer for a unitary decomposer."
  (make-instance 'decomposer
                 :basis-gates basis-gates
                 :num-qubits num-qubits
                 :epsilon0 epsilon0
                 :base-approximations (generate-base-approximations basis-gates num-qubits epsilon0)))

;;; Will be somewhat complicated/tedious. The data structure that will
;;; be used for this representation is still TBD.
(defun generate-base-approximations (basis-gates num-qubits epsilon0)
  "Generates and returns a set of base approximations such that every unitary operator on NUM-QUBITS (all operators in SU(2^NUM-QUBITS)) is within EPSILON0 of some unitary in the set."
  (values basis-gates num-qubits epsilon0))

;;; Will likely involve some kind of search on whatever structure
;;; BASE-APPROXIMATIONS uses.
(defun find-base-approximation (base-approximations u)
  "Base case approximation for a unitary U."
  (values base-approximations u))

(defun sk-iter (decomposer u n)
  "An approximation iteration within the Solovay-Kitaev algorithm at a depth N. Returns a list of integer indices in sign-inverse convention."
  (if (zerop n)
      (find-base-approximation (base-approximations decomposer) u)
      (let* ((comm (gc-decompose u))
             (v-next (sk-iter decomposer (commutator-v comm) (1- n)))
             (w-next (sk-iter decomposer (commutator-w comm) (1- n))))
        (append v-next w-next (dagger v-next) (dagger w-next) (sk-iter decomposer u (1- n))))))

(defun decompose (decomposer unitary epsilon)
  "Decomposes a unitary into a sequence of gates from a finite set of basis gates, such that the resulting decomposition is within EPSILON of the original unitary."
  (let* ((eps0 (epsilon0 decomposer))
         (depth (ceiling (log (/ (log (* epsilon +c-approx+ +c-approx+))
                                 (log (* eps0 +c-approx+ +c-approx+))))
                         (log (/ 3 2))))
         (basis-gates (basis-gates decomposer)))
    (mapcar #'(lambda (x) (basis-gate-from-index basis-gates x)) (sk-iter decomposer unitary depth))))

;;; -------------------------------------------------------------------
;;; --------------FUNCTIONS FOR FINDING GROUP COMMUTATORS--------------
;;; -------------------------------------------------------------------
;;; Overall procedure taken in https://github.com/cmdawson/sk to find
;;; balanced group commutators V and W for a unitary U (below, the '
;;; symbol represents a dagger):
;;;
;;;    1) Convert U to its Bloch vector representation, which is a
;;;       rotation by some theta around an arbitrary axis.
;;;
;;;    2) Find unitaries S, Rx s.t. Rx is a rotation around the X axis
;;;       by theta and SRxS' = U.
;;;
;;;    3) Find the group commutators A, B for Rx s.t. Rx = ABA'B'.
;;;
;;; With A, B, and S, we can set V = SAS' and W = SBS', because then
;;; VWV'W' = SAS'SBS'SA'S'SB'S' = SABA'B'S' = SRxS' = U.

;;; Bloch vector structure and conversions to/from unitary matrices
(defstruct (bloch-vector (:constructor make-bloch-vector))
  "A bloch-vector representation of unitaries as a rotation about an axis on the Bloch sphere."
  (theta 0.0d0 :type double-float)
  (axis #(0 0 0) :type (simple-vector 3)))

;;; Conversions between matrix and bloch vector representations of
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
;;; from bloch vector to matrix, and using this matrix, we can extract
;;; the bloch vector parameters to convert back, which is what the
;;; functions below do.
(defun matrix-to-bloch-vector (mat)
  "Converts a unitary matrix into its bloch-vector representation."
  (let* ((phase-correction (bloch-phase-correction mat))
         (mat (magicl:scale phase-correction mat))
         (x-sin (* -1 (imagpart (magicl:ref mat 0 1))))
         (y-sin (realpart (magicl:ref mat 1 0)))
         (z-sin (imagpart (/ (- (magicl:ref mat 1 1) (magicl:ref mat 0 0)) 2)))
         (cos-theta (realpart (/ (+ (magicl:ref mat 0 0) (magicl:ref mat 1 1)) 2)))
         (sin-theta (sqrt (+ (expt x-sin 2) (expt y-sin 2) (expt z-sin 2))))
         (theta (* 2 (atan sin-theta cos-theta)))
         (axis (make-array 3)))
    (setf (aref axis 0) (if (zerop sin-theta) 0 (/ x-sin sin-theta)))
    (setf (aref axis 1) (if (zerop sin-theta) 0 (/ y-sin sin-theta)))
    (setf (aref axis 2) (if (zerop sin-theta) 0 (/ z-sin sin-theta)))
    (make-bloch-vector :theta theta :axis axis)))

(defun bloch-vector-to-matrix (bv)
  "Converts a bloch-vector to the corresponding unitary matrix."
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

;;; A helper function for calculating the global phase adjustment
;;; needed to put a matrix MAT into the bloch-vector format described
;;; in the comment above, by multiplying MAT by the phase factor.
(defun bloch-phase-correction (mat)
  ;; Both of these quantities should be purely real; if not, return
  ;; the phase factor that makes it real.
  (let* ((diag-sum (+ (magicl:ref mat 0 0) (magicl:ref mat 1 1)))
         (anti-diag-diff (- (magicl:ref mat 1 0) (magicl:ref mat 0 1)))
         (nonzero-num (if (zerop diag-sum) anti-diag-diff diag-sum)))
    (/ (abs nonzero-num) nonzero-num)))

(defun unitary-to-conjugated-x-rotation (u)
  "Given a unitary U, returns unitaries S and Rx such that Rx is a rotation around the X axis by the same angle that U rotates around its axis, and U = SRxS'."
  u)

(defun find-transformation-matrix (a b)
  "Given unitaries A and B, finds the unitary S such that A = SBS'."
  (let* ((a-bv (matrix-to-bloch-vector a))
         (b-bv (matrix-to-bloch-vector b))
         (a-axis (bloch-vector-axis a-bv))
         (b-axis (bloch-vector-axis b-bv))
         (dot-prod (vector-dot-product a-axis b-axis))
         (cross-prod (vector-cross-product b-axis a-axis))
         (result-bv (make-bloch-vector)))
    ;; Only bother finding an axis if the vectors aren't parallel
    (unless (and (zerop (vector-norm cross-prod)) (double~ dot-prod 0))
      (cond ((zerop (vector-norm cross-prod)) nil)
            (t ;; General case
             ;; (vector-normalize cross-prod) Shouldn't be needed
             (setf (bloch-vector-axis result-bv) cross-prod)
             (setf (bloch-vector-theta result-bv) (acos dot-prod)))))
    (bloch-vector-to-matrix result-bv)))

(defun gc-decompose-x-rotation (u)
  "Given a unitary U, returns V and W, two unitaries which are balanced commutators of U (i.e. U = [V, W] = VWV'W'). IMPORTANT: U must be a rotation about the X axis; this is not the general function for any U."
  u)

;;; Don't fully understand this part yet
(defun gc-decompose (u)
  "Find the balanced group commutators V and W for any unitary U."
  u)
