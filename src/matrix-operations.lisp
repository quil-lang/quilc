;;;; matrix-operations.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains a bunch of matrix manipulation routines that will
;;;; eventually be subsumed by MAGICL.  In the meantime, you can at least find
;;;; them all in one place: right here.

(in-package #:cl-quil)

(defun matrix-first-column-equality (x y)
  (check-type x magicl:matrix)
  (check-type y magicl:matrix)
  (and (= (magicl:nrows x) (magicl:nrows y))
       (= (magicl:ncols x) (magicl:ncols y))
       (loop :for j :below (magicl:nrows x)
             :always (double~ (magicl:tref x j 0) (magicl:tref y j 0)))))

(defun scale-out-matrix-phases (mat ref-mat)
  "Attempts to scale out relative phase shifts between the first columns of MAT and REF-MAT."
  (let ((rescale-value 1d0)
        (goodness-value 0d0))
    (dotimes (j (magicl:nrows mat))
      (assert (double~ (abs (magicl:tref mat j 0)) (abs (magicl:tref ref-mat j 0)))
              nil "Matrices do not lie in the same projective class. ~% ~A ~% ~A" mat ref-mat)
      (when (> (abs (magicl:tref mat j 0)) goodness-value)
        (setf goodness-value (abs (magicl:tref mat j 0)))
        (setf rescale-value (/ (magicl:tref ref-mat j 0) (magicl:tref mat j 0)))))
    (magicl:scale mat rescale-value)))

(defun convert-defgate-to-magicl (matrix)
  "Converts a DEFGATE matrix (a row-major LISP single list) to a MAGIC-L special unitary matrix."
  (check-type matrix list)
  (let*
      ((n (round (sqrt (length matrix))))
       (m (from-list matrix (list n n))))
    (magicl:scale (expt (magicl:det m) (/ -1 n)) m)))

(defun extend-by-identity (mat d)
  "Extend a square MAGICL matrix MAT to act on a space of dimension
  d. It is required that MAT is n x n with n <= d. Returns the matrix
  MAT (+) I, where (+) denotes direct sum and I is the (d-n) x (d-n)
  identity matrix."
  (let ((n (ilog2 (magicl:nrows mat))))
    (assert (<= n d)
            (d)
            "The resulting matrix must have at least ~D rows, but only ~D were requested."
            n
            d)
    (kq-gate-on-lines
     mat
     d
     (loop :for i :from (1- n) :downto 0 :collect i))))

(defun matrix-rescale (mat1 mat2)
  "Given square matrices MAT1 and MAT2, returns a pair of matrices of
equal dimensions, extending the smaller matrix by a tensor product
with the identity matrix."
  (let ((size1 (ilog2 (magicl:nrows mat1)))
        (size2 (ilog2 (magicl:nrows mat2))))
    (cond ((< size1 size2)
           (setf mat1 (extend-by-identity mat1 size2)))
          ((< size2 size1)
           (setf mat2 (extend-by-identity mat2 size1))))
    (values mat1 mat2)))

(defun matrix-rescale-and-multiply (mat1 mat2)
  "Multiply two square MAGICL matrices, extending them by the identity
as needed so that they are the same size."
  (multiple-value-bind (mat1 mat2) (matrix-rescale mat1 mat2)
    (magicl:@ mat1 mat2)))

(defun parsed-program-to-logical-matrix (pp &key compress-qubits)
  "Convert a parsed program PP, consisting of only i) gate
 applications ii) trivial control operations (HALT and NOP), and iii)
 pragmas, to an equivalent matrix. If present, rewiring pragmas will
 be applied so that the resulting matrix acts on 'logical', rather
 than 'physical', qubits. When :COMPRESS-QUBITS is enabled (default:
 nil), qubit indices are permuted to minimize matrix size."
  (when compress-qubits
    (setf pp (transform 'compress-qubits pp)))

  (let (initial-rewiring
        final-rewiring)
    (loop :for instr :across (parsed-program-executable-code pp)
          :do (multiple-value-bind (enter exit) (instruction-rewirings instr)
                (setf initial-rewiring (or initial-rewiring enter))
                (setf final-rewiring (or exit final-rewiring))))
    ;; to handle a l2p rewiring, we need to conjugate the "physical"
    ;; matrix of a block by the permutation matrix associated with the
    ;; rewiring. this is somewhat complicated by the fact that rewirings
    ;; when we enter a block and when we exit a block may differ.
    
    ;; TODO: this only works for a single block

    (let ((mat (qvm:parsed-program-unitary-matrix pp)))
      (when initial-rewiring
        (setf mat (matrix-rescale-and-multiply
                   mat
                   (rewiring-to-permutation-matrix-l2p (trim-rewiring initial-rewiring)))))

      (when final-rewiring
        (setf mat (matrix-rescale-and-multiply
                   (rewiring-to-permutation-matrix-p2l (trim-rewiring final-rewiring))
                   mat)))
      mat)))

;; NOTE: the double~ appearing in this routine is anomalous. the SVD routine
;; seems not to reliably return singular values within the double= threshold,
;; even though that's the required semantics. so far, this hasn't bitten us.
(defun kernel (m)
  "Computes the kernel (or 'nullspace') of a matrix m using SVD (slow but accurate)."
  (check-type m magicl:matrix)
  (multiple-value-bind (u s v) (magicl:svd m)
    (declare (ignore u))
    (let ((kernel-entries
           (loop :for i :below (magicl:ncols s) :nconc
              (when (or (>= i (magicl:nrows s))
                        (double~ 0 (magicl:tref s i i)))
                (loop :for j :below (magicl:nrows v) :collect (magicl:tref v i j))))))
      (from-list kernel-entries
                   (list (magicl:nrows v)
                         (/ (length kernel-entries) (magicl:nrows v)))))))

(defun random-special-unitary (n)
  (let ((m (random-unitary (list n n))))
    (magicl:scale! m (expt (magicl:det m) (/ (- n))))))

(defun random-wavefunction (n-qubits)
  "Get a random complex unit vector with (EXPT 2 N-QUBITS) entries."
  (let ((size (expt 2 n-qubits)))
    (loop :repeat size
          :for c := (complex (a:gaussian-random)
                             (a:gaussian-random))
          :collect c :into entries
          :sum (expt (abs c) 2) :into norm-squared
          :finally
             (return (make-array size
                                 :initial-contents
                                 (let ((scaling-factor (/ (sqrt norm-squared))))
                                   (mapcar (lambda (c) (* scaling-factor c)) entries))
                                 :element-type '(complex double-float))))))

(defun orthonormalize-matrix (m)
  "Applies Gram-Schmidt to the columns of a full rank square matrix to produce a unitary matrix."
  (declare (optimize (speed 3)))
  ;; consider each column
  (dotimes (j (magicl:ncols m))
    ;; consider each preceding column, which together form an orthonormal set
    (dotimes (jp j)
      ;; compute the dot product of the columns...
      (let ((scalar
              (loop :for i :below (the fixnum (magicl:nrows m))
                    :sum (the (complex double-float)
                              (* (the (complex double-float) (magicl:tref m i j))
                                 (conjugate (the (complex double-float) (magicl:tref m i jp))))))))
        (declare (type (complex double-float) scalar))
        ;; ... and do the subtraction.
        (dotimes (i (the fixnum (magicl:nrows m)))
          (setf (magicl:tref m i j)
                (- (the (complex double-float) (magicl:tref m i j))
                   (* scalar
                      (the (complex double-float) (magicl:tref m i jp))))))))
    ;; now j is orthogonal to the things that came before it. normalize it.
    (let ((scalar
            (sqrt
             (loop :for i :below (magicl:nrows m)
                   :sum (* (abs (the (complex double-float) (magicl:tref m i j)))
                           (abs (the (complex double-float) (magicl:tref m i j))))))))
      (declare (type double-float scalar))
      (dotimes (i (magicl:nrows m))
        (setf (magicl:tref m i j)
              (/ (the (complex double-float) (magicl:tref m i j))
                 scalar)))))
  m)

(defun kron-matrix-up (matrix n)
  "Thinking of a 2^m x 2^m MATRIX as an operator on m qubits, m < N, produces a 2^n x 2^n matrix acting on qubits 0, ..., m, ..., n-1."
  (let ((m (ilog2 (magicl:nrows matrix))))
    (assert (<= m n))
    (if (= m n)
        matrix
        (kq-gate-on-lines matrix
                          n
                          (a:iota m :start (1- m) :step -1)))))

;; also, some routines for manipulating *vectors*, here expressed as Lisp lists
(defun dot-product (u v)
  (loop
    :for i :in u
    :for j :in v
    :sum (* i (conjugate j))))

(defun vector-difference (u v)
  (loop :for i :in u :for j :in v
        :collect (- i j)))

(defun vector-sum (u v)
  (loop :for i :in u :for j :in v
        :collect (+ i j)))

(defun vector-scale (c u)
  (loop :for i :in u :collect
     (* c i)))

(defun norm (v)
  (abs (sqrt (dot-product v v))))

(defun gram-schmidt (vectors)
  "Performs Gram-Schmidt orthonormalization of the list of complex vectors stored in VECTORS."
  (let ((ret (list)))
    (loop :for v :in vectors :do
      (progn
        (loop :for u :in ret :do
          (setf v (vector-difference v (vector-scale (dot-product v u) u))))
        (when (not (double= 0 (norm v)))
          (push (vector-scale (/ (norm v)) v) ret))))
    ret))

(defun collinearp (vect1 vect2)
  "Tests whether two (complex double) vectors of unit norm are collinear. Returns pair of values: first is whether the vectors were collinear, and the second is the amount by which the dot product of the vectors differs from one."
  ;; TODO Do not use dot product. Instead do component-wise
  ;; calculation of the scalar relation, and check that it is the same
  ;; for all entries.
  ;; TODO Use magicl's dot-product, etc.
  (let ((vect1 (coerce vect1 'list))
        (vect2 (coerce vect2 'list)))
    (assert (double= 1d0 (sqrt (dot-product vect1 vect1))))
    (assert (double= 1d0 (sqrt (dot-product vect2 vect2))))
    ;; rather than computing the full norm, this could be replaced by the
    ;; component-wise calculation (loop ... :always (double= 0d0 (expt ...)))
    ;; which has the potential to terminate early on a negative result
    (let ((abs-overlap (abs (dot-product vect1 vect2))))
      (values (double= 1d0 abs-overlap)
              (- 1d0 abs-overlap)))))

;; also just some general math routines
(defun find-root (f guess &optional (depth-bound 1000))
  "An implementation of Newton's root-finding method."
  (when (minusp depth-bound)
    (return-from find-root (values guess "Depth bound exceeded.")))
  (let ((y0 (funcall f guess)))
    (when (double= y0 0d0)
      (return-from find-root (values guess nil)))
    (let* ((delta +double-comparison-threshold-loose+)
           (y1 (funcall f (+ guess delta)))
           (d (/ (- y1 y0) delta))
           (new-guess (+ guess (/ (- y0) d))))
      (find-root f new-guess (1- depth-bound)))))

;; TODO I have the feeling we have too many matrix comparison routines.
(defun check-instructions-matrix-consistency (instrs-from instrs-to &key (relabeling (minimal-standard-relabeling instrs-from instrs-to)))
  "Compare for equality (up to a phase) the matrix representations of INSTRS-TO and INSTRS-FROM (under the appropriate relabeling)."
  (when (and *compress-carefully*
             (not *enable-approximate-compilation*)
             (notany (a:rcurry #'typep 'state-prep-application)
                     (append instrs-from instrs-to)))
    (let* ((ref-mat (make-matrix-from-quil instrs-from :relabeling relabeling))
           (mat (make-matrix-from-quil instrs-to :relabeling relabeling))
           (kron-size (max (ilog2 (magicl:nrows ref-mat))
                           (ilog2 (magicl:nrows mat))))
           (kroned-mat (kron-matrix-up mat kron-size))
           (kroned-ref-mat (kron-matrix-up ref-mat kron-size)))
      (assert
       (matrix-equality
        kroned-ref-mat
        (scale-out-matrix-phases kroned-mat kroned-ref-mat))))))

(defun print-polar-matrix (m &optional (stream *standard-output*))
  (let ((*print-fractional-radians* nil)
        (*print-polar-form* t)
        (height (magicl:nrows m))
        (width (magicl:ncols m)))
    (format stream "~&")
    (dotimes (i height)
      (dotimes (j width)
        (let* ((z (magicl:tref m i j))
               (abs (if (double= 0d0 (abs z)) 0d0 (abs z)))
               (phase (if (zerop abs) 0d0 (mod (phase z) (* 2 pi)))))
          (format stream "~6,4F∠~6,4F" abs phase))
        (when (< j (1- width))
          (format stream ", ")))
      (format stream "~%"))))

;;;; some leftover stuff from standard-gates.lisp and elsewhere

(defun apply-gate (m instr)
  "Constructs the matrix representation associated to an instruction list consisting of gates. Suitable for testing the output of compilation routines."
  (check-type m magicl:matrix)
  (check-type instr application)
  (a:when-let ((defn (gate-matrix instr)))
    (let* ((mat-size (ilog2 (magicl:nrows m)))
           (size (max mat-size
                      (apply #'max
                             (map 'list (lambda (x) (1+ (qubit-index x)))
                                  (application-arguments instr)))))
           (mat (kq-gate-on-lines defn
                                  size
                                  (mapcar #'qubit-index (application-arguments instr))))
           ;; resize m if necessary
           (m (if (< mat-size size)
                  (kq-gate-on-lines m
                                    size
                                    (a:iota mat-size :start (1- mat-size) :step -1))
                  m)))
      (magicl:@ mat m))))

(defun make-matrix-from-quil (instruction-list &key (relabeling #'identity))
  "If possible, create a matrix out of the instructions INSTRUCTION-LIST using the optional function RELABELING that maps an input qubit index to an output qubit index. If one can't be created, then return NIL.

Instructions are multiplied out in \"Quil\" order, that is, the instruction list (A B C) will be multiplied as if by the Quil program

    A
    B
    C

or equivalently as

    C * B * A

as matrices."
  (flet ((relabel-instr (instr)
           (cond ((typep instr 'application)
                  (unless (every #'is-constant (application-parameters instr))
                    (return-from make-matrix-from-quil nil))
                  (let ((new-instr (copy-instance instr)))
                    (setf (application-arguments new-instr)
                          (mapcar relabeling (application-arguments new-instr)))
                    new-instr))
                 (t instr))))
    (let ((pp (make-instance 'parsed-program
                             :executable-code (map 'vector #'relabel-instr instruction-list))))
      (qvm:parsed-program-unitary-matrix pp))))


(defun su2-on-line (line m)
  "Treats m in SU(2) as either m (x) Id or Id (x) m."
  (kq-gate-on-lines m 2 (list line)))

(defun global-phase-invariant-distance (m1 m2)
  "Measures the 'distance' between two unitary matrices up to a global phase
(see arXiv:2106.07099). Returns D = sqrt(1-|tr(M1 M2^-1)|/n) where 0<=D<=1 and
M1 and M2 are n x n."
  (assert (= (magicl:nrows m1) (magicl:ncols m1)
             (magicl:nrows m2) (magicl:ncols m2)))
  (sqrt (- 1 (/ (abs (magicl:trace (magicl:@ m1 (magicl:dagger m2))))
                (magicl:nrows m1)))))
