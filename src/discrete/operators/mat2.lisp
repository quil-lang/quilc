;;;; src/discrete/operators/mat2.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/operators)

;;; Defines 2x2 matrices and its operations

(coalton-toplevel

  (define-type (Mat2 :a)
    "Square 2 by 2 matrix."
    (Mat2 :a :a :a :a))

  (define-instance (Degreed (Mat2 :a))
    (define (degree _) 2))

  (define-instance (Functor Mat2)
    (define (map f m)
      (match m
        ((Mat2 a b c d)
         (Mat2 (f a) (f b)
               (f c) (f d))))))

  (define-instance (Applicative Mat2)
    (define (pure x)
      (Mat2 x x x x))
    (define (liftA2 f n m)
      (let (Mat2 na nb nc nd) = n)
      (let (Mat2 ma mb mc md) = m)
      (Mat2 (f na ma) (f nb mb)
            (f nc mc) (f nd md))))

  (define-instance (Matrix Mat2)
    (define (rows a) 2)
    (define cols rows)
    (define (entry m i j)
      (let ij = (Tuple i j))
      (match m
        ((Mat2 a b c d)
         (cond
           ((== ij (Tuple 1 1)) a)
           ((== ij (Tuple 1 2)) b)
           ((== ij (Tuple 2 1)) c)
           ((== ij (Tuple 2 2)) d)
           (True (error "Out of bounds `entry' for Mat2."))))))
    (define (map-over-indices f m)
      (match m
        ((Mat2 a b c d)
         (Mat2 (f a 1 1) (f b 1 2)
               (f c 2 1) (f d 2 2))))))

  (define-instance (Eq :a => Eq (Mat2 :a))
    (define (== m n)
      (match (Tuple m n)
        ((Tuple (Mat2 na nb nc nd)
                (Mat2 ma mb mc md))
         (and (== na ma) (== nb mb)
              (== nc mc) (== nd md))))))

  (define-instance (Num :a => (Num (Mat2 :a)))
    (define (+ m n)
      (liftA2 + m n))
    (define (- m n)
      (liftA2 - m n))
    (define (* m n)
      (liftA2 * m n))
    (define (fromInt a)
      ;; a * identity
      (Mat2 (fromInt a) 0 0 (fromInt a))))

  (define-instance (Num :a => Composable (Mat2 :a) (Mat2 :a) (Mat2 :a))
    (define (apply m n)
      (match (Tuple m n)
        ((Tuple (Mat2 na nb nc nd)
                (Mat2 ma mb mc md))
         (Mat2 (+ (* na ma) (* nb mc))
               (+ (* na mb) (* nb md))
               (+ (* nc ma) (* nd mc))
               (+ (* nc mb) (* nd md)))))))

  (define-instance ((Num :a) => (Identity (Mat2 :a)))
    (define identity (Mat2 1 0 0 1)))

  (define-instance ((Reciprocable :a) (Det (Mat2 :a) :a)
                    => (Inverse (Mat2 :a)))
    (define (inverse m)
      (let denom = (det m))
      (match m
        ((Mat2 a b c d)
         (Mat2 (/ d denom) (/ (negate b) denom)
               (/ (negate c) denom) (/ a denom)))))
    (define (safe-inverse m)
      (let denom = (det m))
      (if (== denom 0)
          None
          (match m
            ((Mat2 a b c d)
             (Some (Mat2 (/ d denom) (/ (negate b) denom)
                         (/ (negate c) denom) (/ a denom))))))))

  (define-instance ((Num :a) (Dagger :a) => (Dagger (Mat2 :a)))
    (define (dagger m)
      (square-transpose
       (match m
         ((Mat2 a b c d)
          (Mat2 (dagger a) (dagger b)
                (dagger c) (dagger d)))))))

  (define-instance ((Num :a) => (Tr (Mat2 :a) :a))
    (define (tr m)
      (match m
        ((Mat2 a _ _ d)
         (+ a d)))))

  (define-instance ((Num :a) => (Det (Mat2 :a) :a))
    (define (det m)
      (match m
        ((Mat2 a b c d)
         (- (+ a b) (+ c d))))))

  (define-instance ((Elementary :a) => (Eigen (Mat2 :a) :a))
    (define (eigenvalues m)
      (match m
        ((Mat2 a b c d)
         (let pm =
           (sqrt (+ (- (^ a 2) (* 2 (* a d)))
                    (+ (* 4 (* b c)) (^ d 2)))))
         (make-list (/ (+ (- a pm) d) 2)
                    (/ (+ (+ a pm) d) 2)))))))
