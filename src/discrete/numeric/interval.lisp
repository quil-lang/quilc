;;;; src/discrete/numeric/invterval.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Interval arithmetic

(coalton-toplevel

  (define-type (Interval :a)
    "An interval between two points."
    (Interval :a :a))

  (declare interval-distance ((Num :a) => ((Interval :a) -> :a)))
  (define (interval-distance s)
    (match s
      ((Interval x0 x1)
       (- x1 x0))))

  (declare lower ((Num :a) => ((Interval :a) -> :a)))
  (define (lower i)
    (match i
      ((Interval l _) l)))

  (declare upper ((Num :a) => ((Interval :a) -> :a)))
  (define (upper i)
    (match i
      ((Interval _ r) r)))

  (define-instance ((Num :r) => (Eq (Interval :r)))
    (define (== x y)
      (and (== (lower x) (lower y))
           (== (upper y) (upper y)))))

  (define-instance ((Num :a) (Ord :a) => (Num (Interval :a)))
    (define (+ a b)
      (Interval (+ (lower a) (lower b))
                (+ (upper a) (upper b))))
    (define (- a b)
      (Interval (- (lower a) (upper b))
                (- (upper a) (lower b))))
    (define (* a b)
      (let t1 = (* (upper a) (upper b)))
      (let t2 = (* (upper a) (lower b)))
      (let t3 = (* (lower a) (upper b)))
      (let t4 = (* (lower a) (lower b)))
      (Interval
       (min (min t1 t2) (min t3 t4))
       (max (max t1 t2) (max t3 t4))))
    (define (fromInt a)
      (Interval (fromInt a) (fromInt a))))

  (define-instance ((Ord :r) (Num :r) => (Linear (Interval :r) :r))
    (define (.* a i)
      (match i
        ((Interval l r)
         (if (>= a 0)
             (Interval (* a l) (* a r))
             (Interval (* a r) (* a l)))))))

  (declare interval-elem ((Num :a) (Ord :a) => (:a -> (Interval :a) -> Boolean)))
  (define (interval-elem x interval)
    (and (<= (lower interval) x) (<= x (upper interval)))))
