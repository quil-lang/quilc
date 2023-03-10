;;;; src/discrete/numeric/modulo.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Typed modulo arithmetic where every Modulo type has a unique group order.

;;; Usage: (coalton (fn (x) (the-moduo 4 (modulo x))))
;;;        (coalton ((the-modulo 10) (* 11 -4)))

;; Defines `the-modulo'
(has-singleton Modulo)

(coalton-toplevel

  (repr :transparent)
  (define-type (Modulo :a)
    "Modular arithmetic type."
    ;; For (%Modulo A), A must be less than (extract-single (%Modulo A))
    (%Modulo UFix))

  (declare %from-modulo ((Modulo :a) -> UFix))
  (define (%from-modulo a)
    (match a
      ((%Modulo a) a)))

  (declare %to-modulo (Nat :a => :a -> Integer -> (Modulo :a)))
  (define (%to-modulo nat a)
    (let b = (mod (fromInt a) (fromNat nat)))
    (%Modulo (lisp UFix (b) b)))

  (declare modulo ((Nat :n) (Integral :a) => :a -> (Modulo :n)))
  (define (modulo n)
    "Converts a number into a modular arithmetic number."
    (%to-modulo single (toInteger n)))

  (declare modulo-enforce (Nat :a => Modulo :a -> Modulo :a))
  (define modulo-enforce (compose modulo %from-modulo))

  (declare modulo->ufix ((Nat :a) => (Modulo :a) -> UFix))
  (define modulo->ufix %from-modulo)

  (declare modulo->integer ((Nat :a) => (Modulo :a) -> Integer))
  (define (modulo->integer a)
    (toInteger (modulo->ufix a)))

  (define-instance (Nat :a => (Into (Modulo :a) Integer))
    (define into modulo->integer))

  (declare convert-modulo ((Nat :a) (Nat :b) => (Modulo :a) -> (Modulo :b)))
  (define (convert-modulo a)
    "Conversion from 'A mod n' to some 'A mod m'."
    (modulo (modulo->ufix a)))

  (define-instance (Nat :a => Eq (Modulo :a))
    (define (== a b)
      (== (%from-modulo a) (%from-modulo b))))

  (define-instance (Nat :a => Hash (Modulo :a))
    (define (hash n)
      (hash (modulo->ufix n))))

  (define-instance (Nat :a => Ord (Modulo :a))
    (define (<=> a b)
      (<=> (%from-modulo a) (%from-modulo b))))

  (declare %modulo-64-times
           (Nat :a => :a -> UFix -> Modulo :a -> Modulo :a -> Modulo :a))
  (define (%modulo-64-times _ n a-mod b-mod)
    "With A-MOD and B-MOD less than or equal to N<64, compute (mod (* A B) N)"
    ;; We are gaurenteed U16 as the elements are less than 64^2
    (let to-u16 = (fn (x) (lisp U16 (x) x)))
    (let m = (to-u16 n))
    (let a = (to-u16 (modulo->ufix a-mod)))
    (let b = (to-u16 (modulo->ufix b-mod)))
    ;; We need a 64-bit float for N<64 modular multiplication
    (let x = (the Double-Float (into a)))
    (let y = (* x (into b)))
    (let c = (lisp U16 (y m)
               (cl:nth-value 0 (cl:floor y m))))
    (let ab = (* a b))
    (let cm = (* c m))
    ;; ab - cm mod m
    (if (>= ab cm)
        (modulo (- ab cm))
        (modulo (- (- m ab) cm))))

  (declare %modulo-times (Nat :a => :a -> Modulo :a -> Modulo :a -> Modulo :a))
  (define (%modulo-times _ a b)
    "Equals (mod (* A B) n) where n = (fromNat (extract-single a))."
    (modulo (* (modulo->ufix a) (modulo->ufix b))))

  (declare %modulo-minus
           (Nat :a => :a -> UFix -> (Modulo :a) -> (Modulo :a) -> (Modulo :a)))
  (define (%modulo-minus _ m a-mod b-mod)
    "Equals (mod (- A-MOD B-MOD) n) where n = (fromNat (extract-single A-MOD))."
    (let a = (%from-modulo a-mod))
    (let b = (%from-modulo b-mod))
    (%Modulo
     (if (>= a b)
         (- a b)
         (+ a (- m b)))))

  (declare %modulo-plus (Nat :a => :a
                             -> UFix -> Modulo :a ->  Modulo :a -> Modulo :a))
  (define (%modulo-plus _ m a-mod b-mod)
    "Equals (mod (+ A-MOD B-MOD) n) where n = (fromNat (extract-single A-MOD))."
    (let a = (%from-modulo a-mod))
    (let b = (%from-modulo b-mod))
    (let m-b = (- m b))
    (%Modulo
     (if (>= a m-b)
           (- a m-b)
           (+ a b))))

  (define-instance (Nat :a => (Num (Modulo :a)))
    (define +
      (progn
        (let nat = single)
        (let n = (fromNat nat))
        (%modulo-plus nat n)))
    (define -
      (progn
        (let nat = single)
        (let n = (fromNat nat))
        (%modulo-minus nat n)))
    (define *
      (progn
       (let nat = single)
       (let n = (fromNat nat))
       (cond
         ((< n 64) (%modulo-64-times nat n))
         (True (%modulo-times nat)))))
    (define fromInt modulo))

  (define-instance (Nat :a => (Remainder (Modulo :a)))
    (define (quot a b)
      (%Modulo (quot (%from-modulo a) (%from-modulo b))))
    (define (rem a b)
      (%Modulo (rem (%from-modulo a) (%from-modulo b))))
    (define (quotRem a b)
      (match (quotRem (%from-modulo a) (%from-modulo b))
        ((Tuple a b) (Tuple (%Modulo a) (%Modulo b)))))
    (define (div a b)
      (%Modulo (div (%from-modulo a) (%from-modulo b))))
    (define (mod a b)
      (%Modulo (mod (%from-modulo a) (%from-modulo b))))
    (define (divMod a b)
      (match (divMod (%from-modulo a) (%from-modulo b))
        ((Tuple a b) (Tuple (%Modulo a) (%Modulo b)))))))
