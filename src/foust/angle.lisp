;;;; angle.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/angle
  (:documentation
   "This package represents angles as `Fraction`s, in units of revolutions, i.e., θ ∈ [0,1).")
  (:use
   #:coalton
   #:coalton-prelude)
  (:import-from
   #:coalton-library/math/fraction
   #:numerator
   #:denominator)
  (:import-from
   #:coalton-library/math/elementary
   #:pi)
  (:import-from
   #:coalton-library/math/arith
   #:general/)
  (:import-from
   #:coalton-library/math/integral
   #:divmod)
  (:export
   #:Angle
   #:Angle
   #:angle-order
   #:angle->radians))

(in-package #:cl-quil.foust/angle)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type Angle (%Angle Fraction))

  (declare Angle (Fraction -> Angle))
  (define (Angle fraction) (%Angle (mod fraction 1)))

  (define-instance (Eq Angle)
    (define (== (%Angle fraction-one) (%Angle fraction-two))
      (== fraction-one fraction-two)))

  (define-instance (Default Angle)
    (define (default) (Angle 0/1)))

  (define-instance (Into Angle String)
    (define (into (%Angle fraction))
      (let ((frac (* 2 fraction))
            (tnumer (numerator frac))
            (denom (denominator frac))
            (numer (if (> tnumer denom)
                       (- tnumer (* 2 denom))
                       tnumer)))
        (cond
          ((== 0 numer) "0")
          ((== 1 numer) (if (== 1 denom) "π" (<> "π/" (into denom))))
          ((== -1 numer) (if (== 1 denom) "-π" (<> "-π/" (into denom))))
          (True (mconcat (make-list (into numer) "π/" (into denom))))))))

  (define-instance (Num Angle)
    (define (+ (%Angle fraction-one) (%Angle fraction-two))
      (Angle (+ fraction-one fraction-two)))
    (define (- (%Angle fraction-one) (%Angle fraction-two))
      (Angle (- fraction-one fraction-two)))
    (define (* (%Angle fraction-one) (%Angle fraction-two))
      (Angle (* fraction-one fraction-two)))
    (define fromInt (const (default))))

  (declare angle-order (Angle -> (Optional UFix)))
  (define (angle-order (%Angle fraction))
    "If an `Angle` is an integer `n` multiple of `π/4`, return `(Some n)`, otherwise `None`."
    (match (divmod fraction 1/4)
      ((Tuple order x)
       (if (== 0 x) (Some (unwrap (tryInto (numerator order)))) None)))))

(coalton-toplevel

  (declare angle->radians (Angle -> Double-Float))
  (define (angle->radians (%Angle theta))
    "Convert revolution units stored in an `Angle` object, to a `Double-Float` in radian units."
    (product (make-list 2 pi (general/ (numerator theta) (denominator theta))))))
