;;;; src/discrete/numeric/root2plex.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Defines functions on the type representing the form a + b √2

(coalton-toplevel

  (define-type (Root2plex :a)
    "A ring R adjoin the square root of two:
     R[√2] = {a + b √2 | a,b ∈ R}

     When R is the Reals, R[√2] is isomorphic to perplex numbers.
     When √2∈R, R≃R[√2]"
    (Root2plex :a :a))

  (declare root2-real-part ((Num :a) => ((Root2plex :a) -> :a)))
  (define (root2-real-part z)
    "Represents a in the expression a + b √2."
    (match z
      ((Root2plex a _) a)))

  (declare root2-root-part ((Num :a) => ((Root2plex :a) -> :a)))
  (define (root2-root-part z)
    "Represents b in the expression a + b √2."
    (match z
      ((Root2plex _ b) b)))

  (define-instance (Functor Root2plex)
    (define (map f x)
      (match x
        ((Root2plex a b) (Root2plex (f a) (f b))))))

  (define-instance ((Num :a) => (Eq (Root2plex :a)))
    (define (== p q)
      (and (== (root2-real-part p) (root2-real-part q))
           (== (root2-root-part p) (root2-root-part q)))))

  (define-instance ((Num :a) => (Num (Root2plex :a)))
    (define (+ x y)
      (Root2plex (+ (root2-real-part x) (root2-real-part y))
                 (+ (root2-root-part x) (root2-root-part y))))
    (define (- x y)
      (Root2plex (- (root2-real-part x) (root2-real-part y))
                 (- (root2-root-part x) (root2-root-part y))))
    (define (* x y)
      (match (Tuple x y)
        ((Tuple (Root2plex a b)
                (Root2plex c d))
         (Root2plex (+ (* a c) (* 2 (* b d)))
                    (+ (* a d) (* b c))))))
    (define (fromInt x)
      (Root2plex (fromInt x) 0)))

  (define-instance (Complex :a => (Complex (Root2plex :a)))
    (define (complex a b)
      (makeComplex a b))
    (define (real-part a)
      (complexReal a))
    (define (imag-part a)
      (complexImag a)))

  (define-instance (Into (Complex (Root2plex Integer))
                         (Complex (Root2plex Fraction)))
    (define (into x)
      (complex
       (Root2plex
        (exact/ (root2-real-part (real-part x)) 1)
        (exact/ (root2-root-part (real-part x)) 1))
       (Root2plex
        (exact/ (root2-real-part (imag-part x)) 1)
        (exact/ (root2-root-part (imag-part x)) 1)))))

  (define-instance (Rational :a => (Quantizable (Root2plex :a)))
    (define (floor x)
      (let (Root2plex a b) = x)
      (let a-floor = (floor a))
      (let b-floor = (* (sign b) (isqrt (floor (* 2 (^ b 2))))))
      (let n = (+ a-floor b-floor))
      (let m = (same-type x (fromint n)))
      (cond
        ((<= (+ m 1) x) (+ n 1))
        ((<= m x) n)
        (true (- n 1))))
    (define (proper x)
      (let trunc = (* (sign x) (floor (abs x))))
      (let rem = (- x (fromInt trunc)))
      (Tuple trunc rem))
    (define (ceiling x)
      (negate (floor (negate x)))))

  (define-instance ((Num :a) => (Linear (Root2plex :a) :a))
    (define (.* s v)
      (match v
        ((Root2plex a b)
         (Root2plex (* s a) (* s b))))))

  ;; R[√2] is NOT an inner product space
  (define-instance ((Num :a) => (Inner (Root2plex :a) :a))
    (define (<.> x y)
      (match (Tuple x y)
        ((Tuple (Root2plex xa xb)
                (Root2plex ya yb))
         (- (* xa ya) (* 2 (* xb yb)))))))

  (define-class ((Num :a) => (ExactRoot2 :a))
    "Any numeric type constructed with an exact non-zero representation of √2
must implement the involution ±√2 ↦ ∓√2, otherwise the identity will suffice."
    (root2-conjugate (:a -> :a)))

  (define-instance ((Num :a) => (ExactRoot2 (Root2plex :a)))
    (define (root2-conjugate z)
      "Maps a + b √2 to a - b √2"
      (Root2plex (root2-real-part z) (negate (root2-root-part z)))))

  (define-instance ((Complex :a) => (ExactRoot2 (Complex (Root2plex :a))))
    (define (root2-conjugate z)
      (complex (root2-conjugate (real-part z))
               (root2-conjugate (imag-part z)))))

  (declare root2-reciprocal ((Num :a) (Dividable :a :b)
                             => ((Root2plex :a) -> (Root2plex :b))))
  (define (root2-reciprocal x)
    "The multiplicative invserse of an X ∈ R[√2]. Note that 1/(a + b √2) is
undefined for a = -b√2."
    ;; 1/X = σ(X)/(X σ(X)) = σ(X)/ǁXǁ²
    ;; 1/(a + b √2) = (a - √2b)/(a² - 2b²)
    (map (fn (y) (general/ y (square-norm x)))
         (root2-conjugate x)))

  (define-instance (Dividable (Root2plex Integer) (Root2plex Fraction))
    (define (general/ a b) (* (map fromInt a) (root2-reciprocal b))))

  ;; This is only a field for rationals
  (define-instance  ((Reciprocable :a) => (Reciprocable (Root2plex :a)))
    (define (/ a b)
      (* (map (fn (y) (general/ y 1)) a)
         (root2-reciprocal b)))
    (define (reciprocal x) (root2-reciprocal x)))

  (declare root2 ((Num :a) => (Root2plex :a)))
  (define root2
    "The root 2 unit √2 in the form 0 + 1 √2."
    (Root2plex 0 1))

  (declare root2->floating ((Into :a :b) (Num :a) (Elementary :b) =>
                            ((Root2plex :a) -> :b)))
  (define (root2->floating x)
    (+ (into (root2-real-part x))
       (* (into (root2-root-part x)) (sqrt 2))))

  (declare zroot2->floating ((Elementary :b) =>
                             ((Root2plex Integer) -> :b)))
  (define (zroot2->floating x)
    (+ (fromInt (root2-real-part x))
       (* (fromInt (root2-root-part x)) (sqrt 2))))

  (declare rroot2->floating ((Elementary :b) =>
                             ((Root2plex Fraction) -> :b)))
  (define (rroot2->floating x)
    (+ (/ (fromInt (numerator (root2-real-part x)))
          (fromInt (denominator (root2-real-part x))))
       (* (/ (fromInt (numerator (root2-root-part x)))
             (fromInt (denominator (root2-root-part x))))
          (sqrt 2))))

  (define-instance ((Num :a) (Into :a Single-Float) =>
                    (Into (Root2plex :a) Single-Float))
    (define into root2->floating))

  (define-instance ((Num :a) (Into :a Double-Float) =>
                    (Into (Root2plex :a) Double-Float))
    (define into root2->floating))

  (define-instance ((Num :b) (Into :a :b) => (Into :a (Root2plex :b)))
    (define (into x) (Root2plex (into x) 0)))

  (define-instance (Dagger :a => (Dagger (Root2plex :a)))
    (define (dagger x)
      (match x
        ((Root2plex a b) (Root2plex (dagger a) (dagger b))))))

  (declare signed-square ((Num :a) (Ord :a) => :a -> :a))
  (define (signed-square a) (* a (abs a)))

  (define-instance ((Num :a) (Ord :a) => (Ord (Root2plex :a)))
    (define (<=> x y)
      (match (Tuple x y)
        ((Tuple (Root2plex xa xb)
                (Root2plex ya yb))
         ;; xa + xb√2 <=> ya + yb√2
         ;; sign(xa - ya)(xa - ya)² <=> 2 sign(yb - xb)(yb - xb)²
         (<=> (signed-square (- xa ya))
              (* 2 (signed-square (- yb xb))))))))

  (declare root2-normed-fraction ((Inner (Root2plex :a) :a) (Num :a)
                                  => (:a -> :a -> :a)
                                  -> (Root2plex :a) -> (Root2plex :a)
                                  -> (Root2plex :a)))
  (define (root2-normed-fraction f a b)
    (let dividend = (* a (root2-conjugate b)))
    (let a1 = (root2-real-part dividend))
    (let a2 = (root2-root-part dividend))
    (let divisor = ((the ((Root2plex :a) -> :a) square-norm) b))
    ;; (a1+a2√2)/(b1+b2√2)=(a1+a2√2)(b1-√2b2)/(b1²-2b2²)
    (Root2plex (f a1 divisor)
               (f a2 divisor)))

  (define-instance ((Remainder :a)
                    => (Remainder (Root2plex :a)))
    (define (quot a b)
      (root2-normed-fraction quot a b))
    (define (rem a b)
      (- a (* b (quot a b))))
    (define (quotRem a b)
      (let result-quot = (quot a b))
      (let result-rem = (- a (* b result-quot)))
      (Tuple result-quot result-rem))
    (define (mod a b)
      (root2-normed-fraction mod a b))
    (define (div a b)
      (- a (* b (mod a b))))
    (define (divMod a b)
      (let result-div = (div a b))
      (let result-mod = (- a (* b result-div)))
      (Tuple result-div result-mod)))

  (define-instance ((Factorable :a) (Ord :a)
                    => (Factorable (Root2plex :a)))
    (define (euclid-div a b)
      (root2-normed-fraction euclid-div a b))
    (define (canonical-factor a) (abs a))
    (define (euclid-norm a) (euclid-norm ((the ((Root2plex :a) -> :a)
                                               square-norm) a)))
    (define (euclid-gcd a b) (canonical-gcd a b))))
