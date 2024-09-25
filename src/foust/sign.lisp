;;;; sign.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/sign
  (:documentation
   "This package represents signs, `+` and `-`.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:Sign
   #:Plus
   #:Minus
   #:sign-*))

(in-package #:cl-quil.foust/sign)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :enum)
  (define-type Sign Plus Minus)

  (define-instance (Eq Sign)
    (define (== sign-one sign-two)
      (match (Tuple sign-one sign-two)
        ((Tuple (Plus) (Plus)) True)
        ((Tuple (Minus) (Minus)) True)
        (_ False))))

  (define-instance (Default Sign)
    (define (default) Plus)))

(coalton-toplevel

  (declare sign-* (Sign -> Sign -> Sign))
  (define (sign-* sign-one sign-two)
    "Returns `Plus` if `sign-one` and `sign-two` are equal, otherwise `Minus`."
    (if (== sign-one sign-two) Plus Minus))

  (define-instance (Semigroup Sign)
    (define <> sign-*))

  (define-instance (Monoid Sign)
    (define mempty Plus)))

(coalton-toplevel

  (define-instance (TryInto Char Sign String)
    (define (tryinto char)
      (match char
        (#\+ (Ok Plus))
        (#\- (Ok Minus))
        (_ (Err (mconcat (make-list "`Char` #\\" (into char) " cannot be represented as a `Sign`.

Only the `Char`s #\\+ and #\\- can be represented as `Sign`s.")))))))

  (define-instance (Into Sign Char)
    (define (into sign-s)
      (match sign-s
        ((Plus) #\+)
        ((Minus) #\-))))

  (define-instance (Into Sign String)
    (define (into sign-s)
      (match sign-s
        ((Plus) "+")
        ((Minus) "-")))))
