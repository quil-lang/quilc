;;;; csd.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:cl-quil)

;;; Compute the 2x2 Cosine-Sine decomposition of an unitary matrix using a
;;; modification of the algorithm in: E. S. Gawlik, Yuji. Nakatsukasa, and
;;; B. D. Sutton, “A Backward Stable Algorithm for Computing the CS
;;; Decomposition via the Polar Decomposition,” SIAM J. Matrix Anal. Appl.,
;;; vol. 39, no. 3, pp. 1448–1469, Jan. 2018. DOI:10.1137/18M1182747

(defun m*-diag-general (diag matrix)
  "Returns a newly allocated matrix resulting from the product of DIAG (a diagonal real matrix) with MATRIX (a complex matrix)."
  (declare (type magicl:matrix diag matrix)
           (values magicl:matrix))
  (let* ((m (magicl:matrix-rows diag))
         (n (magicl:matrix-cols diag))
         (k (magicl:matrix-cols matrix))
         (result (magicl:make-zero-matrix m k)))
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

(defun csd-2x1 (a3 a4)
  "Compute the 2x1 Cosine-Sine decomposition of a unitary matrix A partitioned into blocks A1 A2 as shown below:

       ⎡ A3 ⎤        ⎡ A3 ⎤   ⎡ U1     ⎤ ⎡ -S ⎤
If A = ⎢    ⎥, then  ⎢    ⎥ = ⎢        ⎥ ⎢    ⎥ V2ᴴ,
       ⎣ A4 ⎦        ⎣ A4 ⎦   ⎣     U2 ⎦ ⎣  C ⎦

where U1, U2, and V2 are unitary and C^2 + S^2 = I."
  ;; XXX update documentation.
  (multiple-value-bind (w1 h1)
      (polar-decomposition a3 :mode :unitary-hermitian)

    (multiple-value-bind (w2 h2)
        (polar-decomposition a4 :mode :unitary-hermitian)

      (let ((b (m- h2 h1)))
        (multiple-value-bind (lambda2 v2)
            (magicl:hermitian-eig b)
          (declare (ignorable lambda2))

          (let* ((u1 (m- (m* w1 v2)))   ; Absorb the negative sign in the unitary matrix.
                 (u2 (m* w2 v2))
                 (v2h (magicl:conjugate-transpose v2))
                 (s (m* v2h h1 v2))
                 (c (m* v2h h2 v2)))

            (values u1 u2 c s v2h)))))))

(defun csd (matrix p q)
  "Compute the 2x2 Cosine-Sine decomposition of MATRIX (assumed to be unitary and 2n×2n) partitioned into n×n blocks A1 A2 A3 A4 as shown below:

       ⎡ A1  A3 ⎤        ⎡ A1  A3 ⎤   ⎡ U1     ⎤ ⎡ C  -S ⎤ ⎡ V1     ⎤H
If A = ⎢        ⎥, then  ⎢        ⎥ = ⎢        ⎥ ⎢       ⎥ ⎢        ⎥,
       ⎣ A2  A4 ⎦        ⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C ⎦ ⎣     V2 ⎦

where U1, U2, V1, and V2 are unitary and C^2 + S^2 = I."
  ;; XXX update documentation.
  (declare (type magicl:matrix matrix)
           (values magicl:matrix magicl:matrix magicl:matrix magicl:matrix list))

  (let ((m (magicl:matrix-rows matrix)))
    (assert (and (evenp m) (= m (magicl:matrix-cols matrix)))
            (matrix)
            "Invalid matrix size.")

    (assert (and (= p q) (or (= p 1) (= p (/ m 2))))
            (p q)
            "This implementation of the CS decomposition supports equipartitions or partitions with (p, q) = (1, 1) only.")

    (let ((equipartition-p (= p (/ m 2)))
          (a1 (magicl:slice matrix 0 p 0 q))
          (a2 (magicl:slice matrix p m 0 q))
          (a3 (magicl:slice matrix 0 p q m))
          (a4 (magicl:slice matrix p m q m)))

      (multiple-value-bind (u1 u2 c s v2h)
          (csd-2x1 a3 a4)

        (let ((theta (map 'list
                          (lambda (x y)
                            (atan (realpart y) (realpart x)))
                          (subseq (magicl:matrix-diagonal c) 0 p)
                          (subseq (magicl:matrix-diagonal s) 0 p))))

          (multiple-value-bind (v1h y)
              (let ((z (m+ (m* c (magicl:conjugate-transpose u1) a1)
                           (m* s (magicl:conjugate-transpose u2) a2))))
                (polar-decomposition z :mode :hermitian-unitary))
            (declare (ignorable y))

            (flet ((make-complex-1x1-matrix (matrix)
                     "Return complex 1x1 matrix whose sole entry is the first component of the first row of MATRIX."
                     (magicl:make-complex-matrix 1 1 (list (magicl:ref matrix 0 0)))))

              (values (if equipartition-p
                          u1
                          (make-complex-1x1-matrix u1))
                      u2
                      (if equipartition-p
                          v1h
                          (make-complex-1x1-matrix v1h))
                      v2h
                      theta))))))))

(defparameter *lapack-csd* (if (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_" 'magicl.foreign-libraries:liblapack)
                               #'magicl:lapack-csd
                               #'csd)
  "Implementation of the CS decomposition suitable for use in Quilc. Allows us to replace ZUNCSD on systems whose LAPACK does not provide the function.")
