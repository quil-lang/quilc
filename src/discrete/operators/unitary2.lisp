;;;; src/discrete/operators/unitary2.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

;;; Defines U(2) and its operations

(coalton-toplevel
  (define-type (Unitary2 :angle :phase)
    "Unitary group of degree 2."
    (%Unitary2 :angle :phase :phase))

  (define-instance (Degreed (Unitary2 :a :b))
    (define (degree _) 2))

  (declare unitary2 ((Dagger :b) (Num :b)
                     => :a -> :b -> :b -> (Optional (Unitary2 :a :b))))
  (define (unitary2 angle a b)
    (if (== 1 (+ (* a (dagger a)) (* b (dagger b))))
        (Some (%Unitary2 angle a b))
        None))

  (define (unitary2-phase-shift beta u)
    "Apply a phase-shift of BETA to unitary U."
    (match u
      ((%Unitary2 alpha a b)
       (%Unitary2 (<> alpha beta) a b))))

  (declare unitary2-convert ((Into :c :d)
                             (Into :a :b)
                             (Dagger :d)
                             (Num :d)
                             => ((Unitary2 :a :c) -> (Unitary2 :b :d))))
  (define (unitary2-convert u)
    (match u
      ((%Unitary2 alpha a b)
       (match (unitary2 (into alpha) (into a) (into b))
         ((Some u) u)
         ((None) (error "Unitary conversion was invalid."))))))

  (define (unitary2-from-global-phase a)
    (let c = (circleComplex a))
    (match (unitary2 (<> a a) c 0)
      ((Some u) u)
      ((None) (error "Invalid unitary from global phase"))))

  (define (unitary2-topleft m) (match m ((%Unitary2 _ a _) a)))
  (define (unitary2-topright m) (match m ((%Unitary2 _ _ b) b)))
  (define (unitary2-angle m) (match m ((%Unitary2 angle _ _) angle)))

  (define-instance ((Monoid :a) => (Into (SUnitary2 :b) (Unitary2 :a :b)))
    (define (into a)
      (match a
        ((%SUnitary2 a b) (%Unitary2 mempty a b)))))

  (define-instance ((Circle :a :b) (Dagger :b) (Complex :b) (Num :a)
                    => (Composable (Unitary2 :a (Complex :b))
                                   (Unitary2 :a (Complex :b)) (Unitary2 :a (Complex :b))))
    (define (apply m n)
      (match (Tuple m n)
        ((Tuple (%Unitary2 alpha a1 b1)
                (%Unitary2 beta a2 b2))
         (let ((angle-factor (circleComplex beta)))
           (%Unitary2 (<> alpha beta)
                      (- (* a1 a2) (* (* b1 (dagger b2)) angle-factor))
                      (+ (* a1 b2) (* (* b1 (dagger a2)) angle-factor))))))))

  (define-instance ((Circle :a :b) (Dagger :b) (Complex :b) (Num :a)
                    => (Dagger (Unitary2 :a (Complex :b))))
    (define (dagger m)
      (match m
        ((%Unitary2 angle a b)
         (%Unitary2 (invert angle) (dagger a)
                    (negate (* b (dagger (circleComplex angle)))))))))

  (define-instance ((Monoid :a) (Num :b)
                    (Composable (Unitary2 :a :b) (Unitary2 :a :b) (Unitary2 :a :b)) =>
                    (Identity (Unitary2 :a :b)))
    (define identity (%Unitary2 mempty 1 0)))

  (define-instance ((Identity (Unitary2 :a :b)) (Dagger (Unitary2 :a :b)) =>
                    (Inverse (Unitary2 :a :b)))
    (define inverse dagger)
    (define safe-inverse (compose Some inverse)))

  (define-instance ((Eq :a) (Eq :b) (Group :a) (Num :b) => (Eq (Unitary2 :a :b)))
    (define (== m n)
      (match (Tuple m n)
        ((Tuple (%Unitary2 alpha a b)
                (%Unitary2 beta c d))
         (and (== alpha beta) (and (== a c) (== b d)))))))

  (define-instance ((Hash :a) (Hash :b) (Eq (Unitary2 :a :b)) => (Hash (Unitary2 :a :b)))
    (define (hash m)
      (match m
        ((%Unitary2 angle a b) (hash (Tuple3 angle a b))))))

  (define-instance (Into (Unitary2 RootUnity8 (Complex (Root2plex Dyadic)))
                         (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define into unitary2-convert))

  ;; Use native cyclotomic operations (and save us from having intos in type)
  (define-instance (Composable (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
                               (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
                               (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (apply m n)
      (match (Tuple m n)
        ((Tuple (%Unitary2 alpha a1 b1)
                (%Unitary2 beta a2 b2))
         (let ((angle-factor (into (the (Complex (Root2plex Dyadic))
                                        (circleComplex beta)))))
           (%unitary2 (<> alpha beta)
                      (- (* a1 a2) (* (* b1 (dagger b2)) angle-factor))
                      (+ (* a1 b2) (* (* b1 (dagger a2)) angle-factor))))))))

  (define-instance (Dagger (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)))
    (define (dagger m)
      (match m
        ((%Unitary2 angle a b)
         (%Unitary2 (invert angle) (dagger a)
                    (negate (* b (dagger
                                  (into
                                   (the (Complex (Root2plex Dyadic))
                                        (circleComplex angle)))))))))))

  (define-instance ((Circle :a :b) (Dagger :b) (Complex :b) (Num :a)
                    => (Tr (Unitary2 :a (Complex :b)) (Complex :b)))
    (define (tr m)
      (match m
        ((%Unitary2 p a _)
         (+ a (* (circleComplex p) (dagger a)))))))

  (define-instance (Tr (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) (Cyclotomic8 Dyadic))
    (define (tr m)
      (match m
        ((%Unitary2 p a _)
         (+ a (* (into (the (Complex (Root2plex Dyadic)) (circleComplex p))) (dagger a)))))))

  (define-instance ((Circle :a :b) (Dagger :b) (Complex :b) (Num :a)
                    => (Into (Unitary2 :a (Complex :b)) (Mat2 (Complex :b))))
    (define (into m)
      (match m
        ((%Unitary2 p a b)
         (let f = (circleComplex p))
         (Mat2 a b
               (* (negate f) (dagger b))
               (* f (dagger a)))))))

  (define-instance (Into (Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) (Mat2 (Cyclotomic8 Dyadic)))
    (define (into m)
      (match m
        ((%Unitary2 p a b)
         (let f = (into (the (Complex (Root2plex Dyadic)) (circleComplex p))))
         (Mat2 a b
               (* (negate f) (dagger b))
               (* f (dagger a))))))))
