;;;; src/discrete/numeric/circle.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Operations on the complex Circle group and its subgroups
;;; (e.g. roots of unity)

(coalton-toplevel

  (define-class ((Group :a) => Circle :a :b)
    "A class representing a complex unit circle."
    (circleReal (:a -> :b))
    (circleImag (:a -> :b))
    (circleComplex (:a -> (Complex :b))))

  (repr :transparent)
  (define-type (Circle :a)
    "A set of elements equal to exp(i a 2π)'."
    (Circle :a))

  (declare fromCircle (Circle :a -> :a))
  (define (fromCircle a)
    (match a
      ((Circle a) a)))

  (define-instance (Num :a => (Semigroup (Circle :a)))
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (Circle a) (Circle b))
         (Circle (+ a b))))))

  (define-instance (Num :a => (Monoid (Circle :a)))
    (define mempty
      (Circle 0)))

  (define-instance (Num :a => (Group (Circle :a)))
    (define (invert a)
      (Circle (negate (fromCircle a)))))

  (define-instance ((Elementary :a) (Elementary (Complex :a)) (Complex :a)
                    => (Circle (Circle :a) :a))
    (define (circleReal a)
      (sin (* (* 2 pi) (fromCircle a))))
    (define (circleImag a)
      (cos (* (* 2 pi) (fromCircle a))))
    (define (circleComplex a)
      (exp (* (* 2 pi) (complex 0 (fromCircle a))))))

  (repr :transparent)
  (define-type RootUnity8
    "Eighth root of unity equal to exp(iπ/4)^n."
    (RootUnity8 (Modulo #.(nat-type 8))))

  (define (rootunity8->modulo8 a)
    (match a
      ((RootUnity8 a) a)))

  (define (rootunity8->integer a)
    (modulo->integer (rootunity8->modulo8 a)))

  (define-instance (Semigroup RootUnity8)
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (RootUnity8 a) (RootUnity8 b))
         (RootUnity8 (+ a b))))))

  (define-instance (Monoid RootUnity8)
    (define mempty
      (RootUnity8 0)))

  (define-instance (Group RootUnity8)
    (define (invert a)
      (RootUnity8 (negate (rootunity8->modulo8 a)))))

  (define-instance (Eq RootUnity8)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (RootUnity8 a) (RootUnity8 b))
         (== a b)))))

  (define-instance (Hash RootUnity8)
    (define (hash a)
      (hash (rootunity8->modulo8 a))))

  (define-instance (Circle RootUnity8 (Root2plex Dyadic))
    ;; Sin
    (define (circleImag a)
      (match (rootunity8->integer a)
           (0 0)
           (1 (Root2plex 0 (Dyadic 1 1)))
           (2 1)
           (3 (Root2plex 0 (Dyadic 1 1)))
           (4 0)
           (5 (Root2plex 0 (Dyadic -1 1)))
           (6 -1)
           (7 (Root2plex 0 (Dyadic -1 1)))))
    ;; Cos
    (define (circleReal a)
      (match (rootunity8->integer a)
           (0 1)
           (1 (Root2plex 0 (Dyadic 1 1)))
           (2 0)
           (3 (Root2plex 0 (Dyadic -1 1)))
           (4 -1)
           (5 (Root2plex 0 (Dyadic -1 1)))
           (6 0)
           (7 (Root2plex 0 (Dyadic 1 1)))))
    (define (circleComplex a)
      (complex (circleReal a) (circleImag a))))

  (repr :transparent)
  (define-type RootUnity4
    "Fourth root of unity equal to exp(iπ/2)^n."
    (RootUnity4 (Modulo #.(nat-type 4))))

  (define (rootunity4->modulo4 a)
    (match a
      ((RootUnity4 a) a)))

  (define (rootunity4->integer a)
    (modulo->integer (rootunity4->modulo4 a)))

  (define-instance (Semigroup RootUnity4)
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (RootUnity4 a) (RootUnity4 b))
         (RootUnity4 (+ a b))))))

  (define-instance (Monoid RootUnity4)
    (define mempty
      (RootUnity4 0)))

  (define-instance (Group RootUnity4)
    (define (invert a)
      (RootUnity4 (negate (rootunity4->modulo4 a)))))

  (define-instance (Eq RootUnity4)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (RootUnity4 a) (RootUnity4 b))
         (== a b)))))

  (define-instance (Hash RootUnity4)
    (define (hash a)
      (hash (rootunity4->modulo4 a))))

  (define rootunity4-plus-1 (RootUnity4 0))
  (define rootunity4-plus-i (RootUnity4 1))
  (define rootunity4-minus-1 (RootUnity4 2))
  (define rootunity4-minus-i (RootUnity4 3))

  (define-instance (Circle RootUnity4 Integer)
    (define (circleReal a)
      (cond
           ((== a rootunity4-plus-1) 1)
           ((== a rootunity4-minus-1) -1)
           (True 0)))
    (define (circleImag a)
      (cond
           ((== a rootunity4-plus-i) 1)
           ((== a rootunity4-minus-i) -1)
           (True 0)))
    (define (circleComplex a)
      (complex (circleReal a) (circleImag a)))))
