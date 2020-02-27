;;;; matrix-operations.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains a bunch of matrix manipulation routines that will
;;;; eventually be subsumed by MAGICL.  In the meantime, you can at least find
;;;; them all in one place: right here.

(in-package #:cl-quil)


(defconstant +double-comparison-threshold-loose+  1d-5)
(defconstant +double-comparison-threshold-strict+ 5d-11)
(defun double~ (x y)
  "Loose equality of complex double floats, using the absolute threshold stored in +DOUBLE-COMPARISON-THRESHOLD-LOOSE+.  Use this comparison operator when testing for output correctness."
  (let ((diff (abs (- x y))))
    (< diff +double-comparison-threshold-loose+)))
(defun double= (x y)
  "Stringent equality of complex double floats, using the absolute threshold stored in +DOUBLE-COMPARISON-THRESHOLD-STRICT+.  Use this comparison operator when testing for substitution viability."
  (let ((diff (abs (- x y))))
    (< diff +double-comparison-threshold-strict+)))

(defun double>= (&rest args)
  (loop :for (x y) :on args
        :while y
        :always (>= (+ x +double-comparison-threshold-strict+)
                    (- y +double-comparison-threshold-strict+))))

(defun matrix-first-column-equality (x y)
  (check-type x magicl:matrix)
  (check-type y magicl:matrix)
  (and (= (magicl:nrows x) (magicl:nrows y))
       (= (magicl:ncols x) (magicl:ncols y))
       (loop :for j :below (magicl:nrows x)
             :always (double~ (magicl:tref x j 0) (magicl:tref y j 0)))))

(defun matrix-equality (x y)
  (magicl:= x y +double-comparison-threshold-loose+))

(defun scale-out-matrix-phases (mat ref-mat)
  "Attempts to scale out relative phase shifts between the first columns of MAT and REF-MAT."
  (let ((rescale-value 1d0)
        (goodness-value 0d0))
    (dotimes (j (magicl:nrows mat))
      (assert (double~ (abs (magicl:tref mat j 0)) (abs (magicl:tref ref-mat j 0)))
              nil "Matrices do not lie in the same projective class. ~% ~a ~% ~a" mat ref-mat)
      (when (> (abs (magicl:tref mat j 0)) goodness-value)
        (setf goodness-value (abs (magicl:tref mat j 0)))
        (setf rescale-value (/ (magicl:tref ref-mat j 0) (magicl:tref mat j 0)))))
    (magicl:scale mat rescale-value)))

(defun convert-defgate-to-magicl (matrix)
  "Converts a DEFGATE matrix (a row-major LISP single list) to a MAGIC-L special unitary matrix."
  (check-type matrix list)
  (let*
      ((n (round (sqrt (length matrix))))
       (m (magicl:from-list
           matrix
           (list n n)
           :type '(complex double-float))))
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
    (quil::kq-gate-on-lines
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
  ;; to handle a l2p rewiring, we need to conjugate the "physical"
  ;; matrix of a block by the permutation matrix associated with the
  ;; rewiring. this is somewhat complicated by the fact that rewirings
  ;; when we enter a block and when we exit a block may differ.
  (loop
    :with mat := (magicl:const #C(1d0 0d0) '(1 1))
    :with rewiring := (make-rewiring 1)
    :for instr :across (parsed-program-executable-code pp)
    :do (progn
          (flet ((apply-entering-rewiring (raw-rewiring)
                   (let ((trimmed (trim-rewiring raw-rewiring)))
                     (setf mat (reduce #'matrix-rescale-and-multiply
                                       (list (rewiring-to-permutation-matrix-l2p trimmed)
                                             (rewiring-to-permutation-matrix-p2l rewiring)
                                             mat))
                           rewiring trimmed)))
                 (apply-exiting-rewiring (raw-rewiring)
                   (setf rewiring (trim-rewiring raw-rewiring)))
                 (apply-instr (instr)
                   (typecase instr
                     (gate-application
                      (setf mat (apply-gate mat instr)))
                     (halt
                      t)
                     (no-operation
                      t)
                     (pragma
                      t)
                     (otherwise
                      (error "Instruction ~A is not a gate application." instr)))))

            (multiple-value-bind (entering-rewiring exiting-rewiring) (instruction-rewirings instr)
              (when (not (null entering-rewiring))
                (apply-entering-rewiring entering-rewiring))
              (apply-instr instr)
              (when (not (null exiting-rewiring))
                (apply-exiting-rewiring exiting-rewiring))))

          (when (typep instr 'halt)
            (loop-finish)))

    :finally (return (matrix-rescale-and-multiply
                      (rewiring-to-permutation-matrix-p2l rewiring)
                      mat))))

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
      (magicl:from-list kernel-entries
                        (list (magicl:nrows v)
                              (/ (length kernel-entries) (magicl:nrows v)))
                        :input-layout :column-major]))))

(defun random-special-unitary (n)
  (let ((m (magicl:random-unitary (list n n) :type '(complex double-float))))
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
  ;; consider each column
  (dotimes (j (magicl:ncols m))
    ;; consider each preceding column, which together form an orthonormal set
    (dotimes (jp j)
      ;; compute the dot product of the columns...
      (let ((scalar
              (loop :for i :below (magicl:nrows m)
                    :sum (* (magicl:tref m i j)
                            (conjugate (magicl:tref m i jp))))))
        ;; ... and do the subtraction.
        (dotimes (i (magicl:nrows m))
          (setf (magicl:tref m i j)
                (- (magicl:tref m i j)
                   (* scalar
                      (magicl:tref m i jp)))))))
    ;; now j is orthogonal to the things that came before it. normalize it.
    (let ((scalar
            (sqrt
             (loop :for i :below (magicl:nrows m)
                   :sum (* (abs (magicl:tref m i j))
                           (abs (magicl:tref m i j)))))))
      (dotimes (i (magicl:nrows m))
        (setf (magicl:tref m i j)
              (/ (magicl:tref m i j) scalar)))))
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
  "Tests whether two (complex double) vectors of unit norm are collinear."
  (flet ((dot-product (v1 v2 &optional (scalar 1d0))
           (loop :for r :across v1
                 :for s :across v2
                 :sum (* (conjugate r) s scalar))))
    (assert (double= 1d0 (sqrt (dot-product vect1 vect1))))
    (assert (double= 1d0 (sqrt (dot-product vect2 vect2))))
    (let ((peak-abs 1.0d0)
          scalar)
      (loop :for r :across vect1
            :for s :across vect2
            :when (and (not (double= 0d0 (abs r)))
                       (not (double= 0d0 (abs s)))
                       (< (abs (- 1 (abs r))) peak-abs))
            :do (setf peak-abs (abs (- 1 (abs r))))
                (setf scalar (- (phase r) (phase s))))
      ;; rather than computing the full norm, this could be replaced by the
      ;; component-wise calculation (loop ... :always (double= 0d0 (expt ...)))
      ;; which has the potential to terminate early on a negative result
      (and scalar
           (let ((norm (sqrt (dot-product vect1 vect2 (cis scalar)))))
             (double= 1d0 norm))))))


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

(defun matrix-expt (m s &key hermitian?)
  "Computes EXP(M*S).  Only works for unitarily diagonalizable matrices M."
  (multiple-value-bind (d u)
      (if hermitian? (magicl:hermitian-eig m) (magicl:eig m))
    (when *compress-carefully*
      (assert (matrix-equality m
                               (magicl:@ u
                                         (magicl:from-diag d :type '(complex double-float))
                                         (magicl:conjugate-transpose u)))
              ()
              "MATRIX-EXPT failed to diagonalize its input."))
    (let ((dd (magicl:from-diag
               (mapcar (lambda (z) (exp (* z s))) d))))
      (magicl:@ u dd (magicl:conjugate-transpose u)))))

(defun print-polar-matrix (m &optional (stream *standard-output*))
  (let ((*print-fractional-radians* nil)
        (*print-polar-form* t)
        (height (magicl::matrix-rows m))
        (width (magicl::matrix-cols m)))
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
