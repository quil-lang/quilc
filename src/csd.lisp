;;;; csd.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil)

;;; Compute the 2x2 Cosine-Sine decomposition of an unitary matrix following:
;;; E. S. Gawlik, Yuji. Nakatsukasa, and B. D. Sutton, “A Backward Stable
;;; Algorithm for Computing the CS Decomposition via the Polar
;;; Decomposition,” SIAM J. Matrix Anal. Appl., vol. 39, no. 3,
;;; pp. 1448–1469, Jan. 2018. DOI:10.1137/18M1182747

(defun m*-diag-general (diag matrix)
  "Returns a newly allocated matrix resulting from the product of DIAG (a diagonal real matrix) with MATRIX (a complex matrix)."
  (declare (type magicl:matrix diag matrix)
           (values magicl:matrix))
  (let* ((m (magicl:matrix-rows diag))
         (n (magicl:matrix-cols diag))
         (k (magicl:matrix-cols matrix))
         (result (magicl:make-zero-matrix m k)))
    (assert (= n k))
    (dotimes (i (min m n) result)
      (let ((dii (magicl:ref diag i i)))
        (dotimes (j k)
          (setf (magicl:ref result i j) (* dii (magicl:ref matrix i j))))))))

(defun polar-decomposition (matrix &key (mode :unitary-hermitian))
  "Compute the polar decomposition of MATRIX (assumed to be a partial isometry). Return (VALUES Q H) where Q is unitary and H is Hermitian. If MODE is :UNITARY-HERMITIAN, then MATRIX = Q H. Alternatively, if MODE is :HERMITIAN-UNITARY, then MATRIX = H Q."
  (declare (type magicl:matrix matrix)
           (type (member :unitary-hermitian :hermitian-unitary) mode)
           (values magicl:matrix magicl:matrix))

  (multiple-value-bind (u sigma vh)
      (magicl:svd matrix :reduced t)

    (let* ((unitary (m* u vh))
           (hermitian (ecase mode
                        (:unitary-hermitian
                         (m* (magicl:conjugate-transpose vh) (m*-diag-general sigma vh)))
                        (:hermitian-unitary
                         (m* u (m*-diag-general sigma (magicl:conjugate-transpose u)))))))
      (values unitary hermitian))))

(defun csd-2x1 (a1 a2)
  "Compute the 2x1 Cosine-Sine decomposition of a unitary matrix A partitioned into blocks A1 A2 as shown below:

       ⎡ A1 ⎤        ⎡ A1 ⎤   ⎡ U1     ⎤ ⎡ C ⎤
If A = ⎢    ⎥, then  ⎢    ⎥ = ⎢        ⎥ ⎢   ⎥ V1ᴴ,
       ⎣ A2 ⎦        ⎣ A2 ⎦   ⎣     U2 ⎦ ⎣ S ⎦

where U1, U2, and V1 are unitary and C^2 + S^2 = I."
  (multiple-value-bind (w1 h1)
      (polar-decomposition a1 :mode :unitary-hermitian)

    (multiple-value-bind (w2 h2)
        (polar-decomposition a2 :mode :unitary-hermitian)

      (let ((b (m- h2 h1)))
        (multiple-value-bind (lambda1 v1)
            (magicl:hermitian-eig b)
          (declare (ignorable lambda1))

          (let* ((u1 (m* w1 v1))
                 (u2 (m* w2 v1))
                 (v1h (magicl:conjugate-transpose v1))
                 (c (m* v1h h1 v1))
                 (s (m* v1h h2 v1)))
            (values u1 u2 c s v1)))))))

(defun csd-2x2 (matrix)
  "Compute the 2x2 Cosine-Sine decomposition of MATRIX (assumed to be unitary and 2n×2n) partitioned into n×n blocks A1 A2 A3 A4 as shown below:

       ⎡ A1  A3 ⎤        ⎡ A1  A3 ⎤   ⎡ U1     ⎤ ⎡ C  -S ⎤ ⎡ V1     ⎤H
If A = ⎢        ⎥, then  ⎢        ⎥ = ⎢        ⎥ ⎢       ⎥ ⎢        ⎥,
       ⎣ A2  A4 ⎦        ⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C ⎦ ⎣     V2 ⎦

where U1, U2, V1, and V2 are unitary and C^2 + S^2 = I."
  (declare (type magicl:matrix matrix)
           (values magicl:matrix magicl:matrix magicl:matrix magicl:matrix))
  (let* ((m (magicl:matrix-rows matrix))
         (n (/ m 2))
         (a1 (magicl:slice matrix 0 n 0 n))
         (a2 (magicl:slice matrix n m 0 n))
         (a3 (magicl:slice matrix 0 n n m))
         (a4 (magicl:slice matrix n m n m)))

    (multiple-value-bind (u1 u2 c s v1)
        (csd-2x1 a1 a2)

      (let ((z (m- (m* c (magicl:conjugate-transpose u2) a4)
                   (m* s (magicl:conjugate-transpose u1) a3))))
        (multiple-value-bind (v2h y)
            (polar-decomposition z :mode :hermitian-unitary)
          (declare (ignorable y))
          (values u1 u2 c s v1 (magicl:conjugate-transpose v2h)))))))

(defun csd-compat (x p q)
  "Drop-in replacement for LAPACK-CSD. Assumes X a square 2n×2n matrix and p = q = n."
  (assert (and (= p q) (= (* 2 p) (magicl:matrix-rows x))))

  (multiple-value-bind (u1 u2 c s v1 v2)
      (csd-2x2 x)

    (let ((thetas (map 'list
                       (lambda (x y)
                         (atan (realpart y) (realpart x)))
                        (magicl:matrix-diagonal c) (magicl:matrix-diagonal s))))

      (values u1 u2 (magicl:conjugate-transpose v1) (magicl:conjugate-transpose v2) thetas))))

(defparameter *lapack-csd* (if (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_" 'magicl.foreign-libraries:liblapack)
                               #'magicl:lapack-csd
                               #'csd-compat))
