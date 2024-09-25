;;;; pauli-operator-lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust/pauli-operator
  (:documentation
  "This package represents Pauli Operators, I, X, Y, and Z.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:Commute
   #:commute?
   #:mcommute?
   #:PauliOperator
   #:I
   #:X
   #:Y
   #:Z
   #:pauli-operator-*
   #:next-pauli-operator
   #:prev-pauli-operator
   #:levi-civita))

(in-package #:cl-quil.foust/pauli-operator)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (Commute :a)
    "A class of types whose objects may or may not commute with one another."
    (commute? (:a -> :a -> Boolean)))

  (declare mcommute? ((Eq :a) (Semigroup :a) => :a -> :a -> Boolean))
  (define (mcommute? a b)
    "Does (a,b) equal (b,a)?"
    (== (<> a b) (<> b a))))

(coalton-toplevel

  (repr :enum)
  (define-type PauliOperator
    "This type represents the single-qubit Pauli Operators, I, X, Y, and Z, without storing their phases."
    I X Y Z)

  (define-instance (Eq PauliOperator)
    (define (== p q)
      (match (Tuple p q)
        ((Tuple (I) (I)) True)
        ((Tuple (X) (X)) True)
        ((Tuple (Y) (Y)) True)
        ((Tuple (Z) (Z)) True)
        (_ False))))

  (define-instance (Default PauliOperator)
    (define (default) I)))

(coalton-toplevel

  (declare pauli-operator-* (PauliOperator -> PauliOperator -> PauliOperator))
  (define (pauli-operator-* p q)
    "The product of two `PauliOperator`s, ignoring its phase."
    (match (Tuple p q)
      ((Tuple (I) _) q)
      ((Tuple _ (I)) p)
      ((Tuple (X) (X)) I)
      ((Tuple (Y) (Y)) I)
      ((Tuple (Z) (Z)) I)
      ((Tuple (X) (Z)) Y)
      ((Tuple (Z) (X)) Y)
      ((Tuple (X) (Y)) Z)
      ((Tuple (Y) (X)) Z)
      ((Tuple (Y) (Z)) X)
      ((Tuple (Z) (Y)) X)))

  (define-instance (Semigroup PauliOperator)
    (define <> pauli-operator-*))

  (define-instance (Monoid PauliOperator)
    (define mempty I)))

(coalton-toplevel

  (define-instance (Commute PauliOperator)
    (define (commute? p q)
      ;; Two `PauliOperator`s commute if and only if either at least
      ;; one of them is `I` or if they are the same `PauliOperator`.
      (match (Tuple p q)
        ((Tuple (I) _) True)
        ((Tuple _ (I)) True)
        ((Tuple (X) (X)) True)
        ((Tuple (Y) (Y)) True)
        ((Tuple (Z) (Z)) True)
        (_ False))))

  (declare next-pauli-operator (PauliOperator -> PauliOperator))
  (define (next-pauli-operator p)
    "Return the next `PauliOperator`, cyclically: `X` -> `Y` -> `Z`. `I` emits an error."
    (match p
      ((I) (error "`I` has no next pauli operator."))
      ((X) Y)
      ((Y) Z)
      ((Z) X)))

  (declare prev-pauli-operator (PauliOperator -> PauliOperator))
  (define (prev-pauli-operator p)
    "Return the previous `PauliOperator`, cyclically: `X` <- `Y` <- `Z`. `I` emits an error."
    (match p
      ((I) (error "`I` has no previous pauli operator."))
      ((X) Z)
      ((Y) X)
      ((Z) Y)))

  (declare levi-civita (PauliOperator -> PauliOperator -> IFix))
  (define (levi-civita p q)
    "Return 0 if `p` and `q` commute, 1 if they are cyclic, and -1 if they are anti-cyclic,

according to the cycle `X` -> `Y` -> `Z`."
    (match (Tuple p q)
      ((Tuple (I) _) 0)
      ((Tuple _ (I)) 0)
      ((Tuple (X) (X)) 0)
      ((Tuple (Y) (Y)) 0)
      ((Tuple (Z) (Z)) 0)
      ((Tuple (X) (Y)) 1)
      ((Tuple (Y) (Z)) 1)
      ((Tuple (Z) (X)) 1)
      ((Tuple (Z) (Y)) -1)
      ((Tuple (Y) (X)) -1)
      ((Tuple (X) (Z)) -1))))

(coalton-toplevel

  (define-instance (Into PauliOperator Char)
    (define (into p)
      (match p
        ((I) #\I)
        ((X) #\X)
        ((Y) #\Y)
        ((Z) #\Z))))

  (define-instance (Into PauliOperator String)
    (define (into p)
      (match p
        ((I) "I")
        ((X) "X")
        ((Y) "Y")
        ((Z) "Z"))))

  (define-instance (TryInto Char PauliOperator String)
    (define (tryinto char)
      (match char
        (#\I (Ok I))
        (#\X (Ok X))
        (#\Y (Ok Y))
        (#\Z (Ok Z))
        (_ (Err (mconcat (make-list "`Char` #\\" (into char) " cannot be represented as a `PauliOperator`.

Only the `Char`s #\\I, #\\X, #\\Y, and #\\Z can be represented as `PauliOperator`s."))))))))
