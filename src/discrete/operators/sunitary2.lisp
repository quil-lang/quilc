;;;; src/discrete/operators/sunitary2.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

;;; Defines SU(2) and its operations

(coalton-toplevel

  (define-type (SUnitary2 :a)
    "Special Unitary group of degree 2."
    (%SUnitary2 :a :a))

  (define-instance (Degreed (SUnitary2 :a))
    (define (degree _) 2))

  (declare sunitary2 ((Dagger :a) (Num :a) => :a -> :a -> (Optional (SUnitary2 :a))))
  (define (sunitary2 a b)
    (if (== 1 (+ (* a (dagger a)) (* b (dagger b))))
        (Some (%SUnitary2 a b))
        None))

  (define-instance ((Dagger :a) (Num :a) => (Dagger (SUnitary2 :a)))
    (define (dagger m)
      (match m
        ((%SUnitary2 a b)
         (%SUnitary2 (dagger a) (negate b))))))

  (define-instance ((Dagger :a) (Num :a) => (Into (SUnitary2 :a) (Mat2 :a)))
    (define (into m)
      (match m
        ((%SUnitary2 a b)
         (Mat2 a (negate (dagger b))
               b (dagger a))))))

  (define-instance ((Dagger :a) (Num :a)
                    => (Composable (SUnitary2 :a) (SUnitary2 :a) (SUnitary2 :a)))
    (define (apply m n)
      (match (Tuple m n)
        ((Tuple (%SUnitary2 a b)
                (%SUnitary2 c d))
         (%SUnitary2 (- (* a c) (* d (dagger b)))
                     (- (* d (dagger a)) (* b c)))))))

  (define-instance ((Dagger :a) (Num :a) => (Identity (SUnitary2 :a)))
    (define identity (%SUnitary2 1 0)))

  (define-instance ((Dagger :a) (Num :a) => (Inverse (SUnitary2 :a)))
    (define inverse dagger)
    (define (safe-inverse m)
      ;; By definition of unitary matrix
      (Some (inverse m))))

  (define-instance ((Dagger :a) (Num :a) => (Tr (SUnitary2 :a) :a))
    (define (tr m)
      (match m
        ((%SUnitary2 a _)
         (+ a (dagger a))))))

  (define-instance ((Dagger :a) (Num :a) => (Det (SUnitary2 :a) :a))
    (define (det _) 1)))
