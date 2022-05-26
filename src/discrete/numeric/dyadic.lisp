;;;; src/discrete/numeric/dyadic.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;; Arbitrarily sized dyadic rationals

(coalton-toplevel
  (define-type Dyadic
    "Dyadic rationals are {n/2^m | n,m∈ℤ}"
    (Dyadic Integer Integer))

  (define (dyadic-numerator d)
    "Maps a dyadic n/2^m to n"
    (match d
      ((Dyadic n _) n)))

  (define (dyadic-exponent d)
    "Maps a dyadic n/2^m to m"
    (match d
      ((Dyadic _ k) k)))

  (define (dyadic-simplify d)
    "Simplifies the fraction formed by Dyadic"
    ;; a / 2^k = 2^x/2^k = 1/2^(k-x) = 2^(x-k)
    (let r = (the Fraction (into d)))
    (match (exact-ilog 2 (denominator r))
      ((Some k) (Dyadic (numerator r) k))
      ((None) d)))

  (define-instance (Into Dyadic Fraction)
    (define (into a)
      (match a
        ((Dyadic n k)
         (/ (fromInt n) (^^ 2 k))))))

  (define-instance (Quantizable Dyadic)
    (define (proper x)
      (match x
        ((Dyadic n k)
         (match (quotRem n (^ 2 k))
           ((Tuple q r)
            (Tuple q (Dyadic r k)))))))
    (define (floor x)
      (floor (the Fraction (into x))))
    (define (ceiling x)
      (ceiling (the Fraction (into x)))))

  (define-instance (Real Dyadic)
    (define (real-approx a x)
      (real-approx a (the Fraction (into x)))))

  (define-instance (Rational Dyadic)
    (define (to-fraction x) (into x))
    (define (best-approx x) (into x)))

  (declare dyadic-compare-function ((Integer -> Integer -> :a)
                                    -> Dyadic -> Dyadic -> :a))
  (define (dyadic-compare-function f a b)
    (match (Tuple a b)
      ((Tuple (Dyadic n k)
              (Dyadic m j))
       ;; n / 2^k `f' m / 2^j
       ;; n 2^j `f' m 2^k
       ;; n 2^j / 2^l `f' m 2^k / 2^l
       ;; n 2^(j-l) `f' m 2^(k-l) where l is min(j k)
       (progn
         (let l = (min j k))
         (f (bits:shift (- j l) n)
            (bits:shift (- k l) m))))))

  (define-instance (Eq Dyadic)
    (define (== a b)
      (dyadic-compare-function == a b)))

  (define-instance (Ord Dyadic)
    (define (<=> a b)
      (dyadic-compare-function <=> a b)))

  (define-instance (Hash Dyadic)
    (define (hash a)
      (match (dyadic-simplify a)
        ((Dyadic n k)
         (combine-hashes (hash n) (hash k))))))

  (declare dyadic-group-function ((Integer -> Integer -> Integer)
                                  -> Dyadic -> Dyadic -> Dyadic))
  (define (dyadic-group-function f a b)
    (match (Tuple a b)
      ((Tuple (Dyadic n k)
              (Dyadic m j))
       (if (> k j)
           (Dyadic (f n (bits:shift (- k j) m)) k)
           (Dyadic (f (bits:shift (- j k) n) m) j)))))

  (define-instance (Num Dyadic)
    (define (+ a b)
      (dyadic-group-function + a b))
    (define (- a b)
      (dyadic-group-function - a b))
    (define (* a b)
      (match (Tuple a b)
        ((Tuple (Dyadic n k)
                (Dyadic m j))
         (Dyadic (* n m) (+ j k)))))
    (define (fromInt x) (Dyadic x 0)))

  (define-instance (Dagger Dyadic)
    (define dagger id))

  (define-instance (Into Integer Dyadic)
    (define into fromInt))

  (define-instance (Into Dyadic (Cyclotomic8 Dyadic))
    (define (into x)
      (Cyclotomic8 0 0 0 x)))

  (define-class (Num :a => (OneHalf :a))
    "Types with exact representations of one half"
    (oneHalf :a))

  (define-instance (OneHalf Dyadic)
    (define oneHalf (Dyadic 1 1)))

  (define-instance (OneHalf Fraction)
    (define oneHalf (exact/ 1 2)))

  (define-instance (Complex Dyadic)
    (define (complex a b)
      (makeComplex a b))
    (define (real-part a)
      (complexReal a))
    (define (imag-part a)
      (complexImag a))))
