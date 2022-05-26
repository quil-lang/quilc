;;;; src/discrete/operators/c2root2.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

;;; Instead of repeatedly multiplying/dividing by √2, define the asscoiated
;;; linear transformation, "compose" it repeadedly, and then apply it.

(coalton-toplevel
  (define-type (C2Root2 :a)
    "Multiplication of a Cyclotomic8 by Root2"
    (Times1 :a)
    (TimesRoot2 :a))

  (declare divide-root2 (OneHalf :a => C2Root2 :a))
  (define divide-root2 (TimesRoot2 oneHalf))

  ;; Up to the dyadic coefficient, this is a Cyclic group of order 4
  (define-instance (Num :a => (Composable (C2Root2 :a)
                                          (C2Root2 :a) (C2Root2 :a)))
    (define (apply a b)
      (match (Tuple a b)
        ;; Times1
        ((Tuple (Times1 x) (Times1 y)) (Times1 (* x y)))

        ((Tuple (Times1 x) (TimesRoot2 y)) (TimesRoot2 (* x y)))
        ((Tuple (TimesRoot2 x) (Times1 y)) (TimesRoot2 (* x y)))

        ((Tuple (TimesRoot2 x) (TimesRoot2 y)) (Times1 (* 2 (* x y)))))))

  ;; This can be represented by 4x4 matrices:
  ;; matrix([0,1,0,-1],[1,0,1,0],[0,1,0,1],[-1,0,1,0])
  (define-instance (Num :a => (Composable (C2Root2 :a)
                                          (Cyclotomic8 :a) (Cyclotomic8 :a)))
    (define (apply x y)
      (match y
        ((Cyclotomic8 a b c d)
         (match x
           ((Times1 coeff)
            (Cyclotomic8 (* coeff a) (* coeff b) (* coeff c) (* coeff d)))
           ((TimesRoot2 coeff)
            (Cyclotomic8 (* coeff (- b d)) (* coeff (+ c a))
                         (* coeff (+ b d)) (* coeff (- c a)))))))))

  (define-instance ((Num :a) => (Identity (C2Root2 :a)))
    (define identity (Times1 1)))
  (declare times-root2^^ ((OneHalf :a)
                    => Integer -> (Cyclotomic8 :a) -> (Cyclotomic8 :a)))
  (define (times-root2^^ n omega)
    "Takes an OMEGA and multiplies it by √2^n"
    (if (> n 0)
        (constructor-act (@^ (TimesRoot2 1) n) omega)
        (constructor-act (@^ divide-root2 (negate n)) omega))))
