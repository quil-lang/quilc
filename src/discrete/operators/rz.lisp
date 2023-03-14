;;;; src/discrete/operators/rz.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/operators)

(named-readtables:in-readtable coalton:coalton)

;;; Defines an RZ(θ) gate, mainly used for converting to Mat2

(coalton-toplevel

  (repr :transparent)
  (define-type (Rz :a)
    "Rotation on z-axis of Bloch Sphere"
    (Rz :a))

  (define (rz-angle m)
    "For an RZ(θ) return θ"
    (match m
      ((Rz theta) theta)))

  (define-instance ((Elementary :a) (Elementary (Complex :a))
                    (Complex :a) (Reciprocable :a)
                    => (Into (Rz :a) (Mat2 (Complex :a))))
    (define (into m)
      (match m
        ((Rz theta)
         (let ((coeff (exp (* (complex 0 (/ 1 2)) (complex theta 0)))))
           (Mat2 (conjugate coeff) 0 0 coeff))))))

  (define-instance ((Num :a) => (Composable (Rz :a) (Rz :a) (Rz :a)))
    (define (apply a b)
      (Rz (+ (rz-angle a) (rz-angle b)))))

  (define-instance ((Num :a) => (Identity (Rz :a)))
    (define identity (Rz 0)))

  (define-instance ((Dagger :a) (Num :a) => (Dagger (Rz :a)))
    (define (dagger m)
      (Rz (negate (dagger (rz-angle m))))))

  (define-instance ((Elementary :a) => (Inverse (Rz :a)))
    (define (inverse m)
      ;; (θ = 4 π n - β, n ∈ ℤ)
      (Rz (- (* pi 4) (rz-angle m))))
    (define (safe-inverse m)
      ;; det = 1
      (Some (inverse m))))

  (define-instance ((Complex :a) (Dagger :a) (Reciprocable :a)
                    (Elementary (Complex :a))
                    => (Eigen (Rz (Complex :a)) (Complex :a)))
    (define (eigenvalues m)
      (make-list (exp (* (complex 0 (/ -1 2)) (rz-angle m)))
                 (exp (* (complex 0 (/ 1 2)) (rz-angle m)))))))
