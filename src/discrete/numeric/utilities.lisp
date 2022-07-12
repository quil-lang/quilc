;;;; src/discrete/numeric/utilities.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil/discrete/numeric)

(coalton-toplevel

  (define (blum-blum-shub p q s)
    "Given two primes P and Q, as well as a seed S, create psuedu-random number
generator that takes an N and returns a value between 0 and 1"
    (let m = (* p q))
    (let lambda-m = (lcm (- p 1) (- q 1)))
    ;; Functional version
    (fn (n)
      (exact/ (expt-mod s (expt-mod 2 n lambda-m) m) m)))

  (define psuedo-random
    "Given a N return a psuedo-random number between 0 and 1."
    (blum-blum-shub 11 23 3))

  (define-class (Monoid :a => Group :a)
    "Type with a group-like structure (i.e. an invertable Monoid)"
    (invert (:a -> :a)))

  (define-class (ConstructComplex :a)
    (makeComplex (:a -> :a -> (Complex :a)))
    (complexReal (Complex :a -> :a))
    (complexImag (Complex :a -> :a)))

  (define-instance (Num :a => (ConstructComplex :a))
    (define (makeComplex a b)
      (coalton-library/math/complex::%Complex a b))
    (define (complexReal a)
      (match a
        ((coalton-library/math/complex::%Complex a _) a)))
    (define (complexImag a)
      (match a
        ((coalton-library/math/complex::%Complex _ b) b))))

  (declare same-type (:a -> :a -> :a))
  (define (same-type _ x) x)

  (declare fraction->reciprocable (Reciprocable :a => Fraction -> :a))
  (define (fraction->reciprocable x)
      (/ (fromInt (numerator x)) (fromInt (denominator x))))

  (declare expt-mod (Integer -> Integer -> Integer -> Integer))
  (define (expt-mod a x n)
    "Defines a less naïve (mod (expt A X) N) for non-negative X N."
    (when (< x 0)
      (error "Negative power in `expt-mod'."))
    (when (< n 0)
      (error "Negative base in `expt-mod'."))
    (let ((expt-mod-rec
            (fn (base power result)
              (if (> power 0)
                  (expt-mod-rec (mod (* base base) n)
                                (ash power -1)
                                (if (odd? power)
                                    (mod (* result base) n)
                                    result))
                  result))))
      (expt-mod-rec (mod a n) x (mod 1 n))))

  (declare exact-ilog (Integer -> Integer -> (Optional Integer)))
  (define (exact-ilog b x)
    "Computes the logarithm with base B of X only if the result is an integer."
    (if (== 0 (mod b x))
        (Some (ilog b x))
        None))

  (declare fractional-factor ((Functor :a) (Num (:a Fraction))
                              => (:a Fraction) -> Integer))
  (define (fractional-factor a)
    "Given an A with Fraction coefficients, for fractions with prime-factorable
numerators return the least common denominator of all said fractions; or for
fractions with denominators of 1, return the negative least common numerator."
    (let factor = (cell:new 0))
    (let set-factor! =
      (fn (x)
        (let f = (cell:read factor))
        (let top = (numerator x))
        (let bottom = (denominator x))
        (cond
          ((and (<= f 0) (== bottom 1))
           (cell:write! factor (negate (gcd top f))))
          ((== f 0)
           (cell:write! factor bottom))
          ((/= f bottom)
           (cell:write! factor (lcm f bottom))))
        Unit))
    (map set-factor! a)
    (cell:read factor))

  (declare integer-fractional? ((Functor :a) => (:a Fraction) -> Boolean))
  (define (integer-fractional? x)
    "Checks if fractional coefficients can all be represented with integers."
    (let ret = (cell:new True))
    (let integer-fraction!? =
      (fn (y)
        (if (== 1 (denominator y))
            True
            (return (cell:write! ret False)))))
    (map integer-fraction!? x)
    (cell:read ret))

  (declare smallest-dividing-exponent
           ((Functor :a) (Num (:a Integer)) (Reciprocable (:a Fraction)) =>
            (:a Fraction) -> (:a Integer) -> (Optional Integer)))
  (define (smallest-dividing-exponent a b)
    "Find the smallest integer x such that AB^x has integer coefficients (SDE)."
    (let b-frac = (map (fn (x) (exact/ x 1)) b))
    (when (or (== b-frac a) (== b-frac (negate a))) (return (Some 1)))
    (when (or (== a 1) (== a 0)) (return (Some 0)))
    (when (or (== b-frac 1) (== b-frac 0)) (return None))
    (let k = (* 2 (fractional-factor a)))
    (let ab^i = (cell:new a))
    (let ((compute-exponent
            (fn (i)
              (cond
                ((< k i) None)
                ((integer-fractional?
                  (cell:write! ab^i
                               (* (cell:read ab^i) (^^ b-frac (sign i)))))
                 (Some i))
                (True (compute-exponent (+ i (sign k))))))))
      (compute-exponent (sign k))))

  (define-class (Num :a => (Factorable :a))
    "A Euclidean domain such that with a Euclidean function n
    a = (+ (* b (euclid-div a b) (euclid-mod a b))
    where (== (euclid-mod a b) fromInteger 0) (< (n (euclid-mod a b)) (n b))

    For example, `abs' is a Euclidean function for integer, and `extension-norm'
    is one for Guassian integers.

    The `euclid-gcd' is an algorithm equivalent per type to Euclid's Algorithm
defined by repeated calls of `euclid-mod' which on Reals is a rounded modulo
defined by `euclid-div' which again on reals is centered/rounded division.
Euclid's Algorithm can admit multiple factors unique up to multiplication of
units, `canonical-factor' refines this choice - typically with the predicate:
toInteger (canonical-factor x) = max({toInteger(x*u) | u is a unit})."
    ;; Multiplication of units s.t.
    ;; toInteger (canonical-factor x) = max({toInteger(x*u) | u is a unit})
    (canonical-factor (:a -> :a))
    (euclid-norm (:a -> Integer))
    ;; Centered division for Ord (e.g. round/)
    (euclid-div (:a -> :a -> :a))
    ;; Euclid's Algorithm using `euclid-mod' or equivantly more efficient.
    (euclid-gcd (:a -> :a -> :a)))

  (declare euclid-field-norm ((Num :a) => :a -> Integer))
  (define (euclid-field-norm x)
    (if (== x 0) 0 1))

  (declare euclid-mod ((Factorable :a) => :a -> :a -> :a))
  (define (euclid-mod a b)
    (- a (* b (euclid-div a b))))

  (declare general-gcd ((Factorable :a) => :a -> :a -> :a))
  (define (general-gcd a b)
    "The \"greatest\" common denominator up to a unit (e.g. for integers this
means it may be negative)."
    (if (== b 0) a
        (general-gcd b
                     (euclid-mod a b))))

  (declare canonical-gcd ((Factorable :a) => :a -> :a -> :a))
  (define (canonical-gcd a b)
    "The \"greatest\" common denominator up to a unit with `canonical-factor'
deciding a factor equivalent by multiplication of units from `general-gcd'.
For example if `canonical-factor' is `abs' (*±1) then this is the standard GCD."
    (canonical-factor (general-gcd a b)))  

  (define-instance (Factorable Integer)
    (define canonical-factor abs)
    (define euclid-norm abs)
    (define (euclid-div a n)
      (round/ a n))
    (define (euclid-gcd a n)
      (gcd a n)))

  (define-instance (Factorable Fraction)
    (define canonical-factor abs)
    (define euclid-norm (compose floor abs))
    (define (euclid-div a b)
      (fromInt
       (euclid-div
        (* (numerator a) (denominator b))
        (* (numerator b) (denominator a)))))
    (define euclid-gcd canonical-gcd))

  (define-instance (Factorable (Complex Integer))
    (define (canonical-factor x)
      (let a = (real-part x))
      (let b = (imag-part x))
      (cond
        ((> 0 a)
         (negate x))
        ((and (> b 0) (< a b))
         (complex b (negate a)))
        ((and (< b 0) (> a b))
         (complex (negate b) a))
        (True x)))
    (define (euclid-norm a)
      (+ (* (real-part a) (real-part a))
         (* (imag-part a) (imag-part a))))
    (define (euclid-div x y)
      (let dividend = (* x (conjugate y)))
      (let a = (real-part dividend))
      (let b = (imag-part dividend))
      (let divisor = (euclid-norm y))
      (complex (euclid-div a divisor)
               (euclid-div b divisor)))
    (define (euclid-gcd x y) (general-gcd x y))))
