;;;; src/discrete/numeric/cyclotomic8.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/numeric)

(named-readtables:in-readtable coalton:coalton)

;;; Defines functions on the type of cyclotomic polynomials of degree 8

(coalton-toplevel

  (define-type (Cyclotomic8 :a)
    "A ring R adjoin the 8th root of unity (denoted ω)
     ω = exp(iπ/4) = (1 + i)/√2
     R[ω] = {aω³+bω²+cω+d | a,b,c,d ∈ R}
     Or equivalently, the cyclotomic ring of degree 8."
    (Cyclotomic8 :a :a :a :a))

  (define-instance (Functor Cyclotomic8)
    (define (map f x)
      (match x
        ((Cyclotomic8 a b c d) (Cyclotomic8 (f a) (f b) (f c) (f d))))))

  (declare omega-cubed-term ((Num :a) => ((Cyclotomic8 :a) -> :a)))
  (define (omega-cubed-term p)
    "Returns the a term in the expression aω³+bω²+cω+d."
    (match p
      ((Cyclotomic8 a _ _ _) a)))

  (declare omega-squared-term ((Num :a) => ((Cyclotomic8 :a) -> :a)))
  (define (omega-squared-term p)
    "Returns the b term in the expression aω³+bω²+cω+d."
    (match p
      ((Cyclotomic8 _ b _ _) b)))

  (declare omega-term ((Num :a) => ((Cyclotomic8 :a) -> :a)))
  (define (omega-term p)
    "Returns the c term in the expression aω³+bω²+cω+d."
    (match p
      ((Cyclotomic8 _ _ c _) c)))

  (declare omega-empty-term ((Num :a) => ((Cyclotomic8 :a) -> :a)))
  (define (omega-empty-term p)
    "Returns the d term in the expression aω³+bω²+cω+d."
    (match p
      ((Cyclotomic8 _ _ _ d) d)))

  (define-instance ((Eq :a) => (Eq (Cyclotomic8 :a)))
    (define (== p q)
      (match (Tuple p q)
        ((Tuple (Cyclotomic8 a1 b1 c1 d1)
                (Cyclotomic8 a2 b2 c2 d2))
         (and (== a1 a2) (== b1 b2) (== c1 c2) (== d1 d2))))))

  (define-instance ((Hash :a) => (Hash (Cyclotomic8 :a)))
    (define (hash p)
      (match p
        ((Cyclotomic8 a b c d)
         (hash:combine-hashes
          (hash:combine-hashes (hash a) (hash b))
          (hash:combine-hashes (hash c) (hash d)))))))

  (define-instance ((Num :a) => (Num (Cyclotomic8 :a)))
    (define (+ p q)
      (match (Tuple p q)
        ((Tuple (Cyclotomic8 a1 b1 c1 d1)
                (Cyclotomic8 a2 b2 c2 d2))
         (Cyclotomic8 (+ a1 a2) (+ b1 b2) (+ c1 c2) (+ d1 d2)))))
    (define (- p q)
      (match (Tuple p q)
        ((Tuple (Cyclotomic8 a1 b1 c1 d1)
                (Cyclotomic8 a2 b2 c2 d2))
         (Cyclotomic8 (- a1 a2) (- b1 b2) (- c1 c2) (- d1 d2)))))
    (define (* p q)
      (match (Tuple p q)
        ((Tuple (Cyclotomic8 a1 b1 c1 d1)
                (Cyclotomic8 a2 b2 c2 d2))
         (Cyclotomic8
          (+ (+ (* c1 b2) (* b1 c2)) (+ (* a1 d2) (* d1 a2)))
          (+ (- (* c1 c2) (* a1 a2)) (+ (* b1 d2) (* d1 b2)))
          (- (+ (* d1 c2) (* c1 d2)) (+ (* b1 a2) (* a1 b2)))
          (- (- (* d1 d2) (* a1 c2)) (+ (* b1 b2) (* c1 a2)))))))
    (define (fromInt x)
      (Cyclotomic8 0 0 0 (fromInt x))))

  (declare cyclotomic8-norm (Num :a => (Cyclotomic8 :a)-> :a))
  (define (cyclotomic8-norm x)
    (match x
      ((Cyclotomic8 a b c d)
       ;; (a^2+b^2+c^2+d^2)^2−2(a*b+b*c+c*d−d*a)^2
       (- (^ (+ (+ (^ a 2) (^ b 2)) (+ (^ c 2) (^ d 2))) 2)
          (* 2 (^ (+ (+ (* a b) (* b c)) (- (* c d) (* d a))) 2))))))

  (declare cyclotomic8-reciprocal
           ((Dividable :a :b) (Num :a) (Num :b)
            => ((Cyclotomic8 :a) -> (Optional (Cyclotomic8 :b)))))
  (define (cyclotomic8-reciprocal x)
    (let divisor = (cyclotomic8-norm x)) ; -a^4
    (if (== divisor 0)
        None
        (match x
          ((Cyclotomic8 a b c d)
           (let (;; d^3+2*a*c*d+b^2*d-b*c^2+a^2*b
                 (e1 (+ (+ (+ (^ d 3) (* 2 (* a (* c d))))
                           (- (* (^ b 2) d)
                              (* b (^ c 2))))
                        (* (^ a 2) b)))
                 ;; a*d^2-2*b*c*d+c^3+a^2*c-a*b^2
                 (e2 (- (+ (- (* a (^ d 2)) (* 2 (* b (* c d))))
                           (+ (^ c 3)
                              (* (^ a 2) c)))
                        (* a (^ b 2))))
                 ;; b*d^2-c^2*d+a^2*d-2*a*b*c+b^3
                 (e3 (+ (+ (- (* b (^ d 2)) (* (^ c 2) d))
                           (- (* (^ a 2) d)
                              (* 2 (* a (* b c)))))
                        (^ b 3)))
                 ;; c*d^2+2*a*b*d+a*c^2-b^2*c+a^3
                 (e4 (+ (+ (+ (* c (^ d 2)) (* 2 (* a (* b d))))
                           (- (* a (^ c 2))
                              (* (^ b 2) c)))
                        (^ a 3))))
             (Some
              (map (fn (dividend) (general/ dividend divisor))
                   (Cyclotomic8 (negate e2) (negate e3) (negate e4) e1))))))))

  (define-instance (Reciprocable :a => Reciprocable (Cyclotomic8 :a))
    (define (/ a b)
      (match (cyclotomic8-reciprocal b)
        ((Some b-recip)
         (* (map (fn (x) (general/ x 1)) a)
            b-recip))
        ((None) (error "Can't divide by zero."))))
    (define (reciprocal b)
      (match (cyclotomic8-reciprocal b)
        ((Some recip) recip)
        ((None) (error "Can't divide by zero.")))))

  (declare cyclotomic8-square-modulus ((Num :a)
                                       => (Cyclotomic8 :a) -> (Root2plex :a)))
  (define (cyclotomic8-square-modulus x)
    "Computes XX^† in which will take a R[ω] to R[√2] "
    (match x
      ((Cyclotomic8 a b c d)
       (Root2plex
        (+ (+ (^ a 2) (^ b 2)) (+ (^ c 2) (^ d 2)))
        (+ (- (* c d) (* a d)) (+ (* b c) (* a b)))))))

  (declare eighth-root-of-unity ((Num :a) => (Cyclotomic8 :a)))
  (define eighth-root-of-unity
    (Cyclotomic8 0 0 1 0))

  (define-instance (Into Integer (Cyclotomic8 Integer))
    (define (into a)
      (fromInt a)))

  (define-instance (Into (Cyclotomic8 Integer) (Cyclotomic8 Fraction))
    (define (into x)
      (match x
        ((Cyclotomic8 a b c d)
         (Cyclotomic8
          (exact/ a 1) (exact/ b 1)
          (exact/ c 1) (exact/ d 1))))))

  (define-instance ((Num :a) (Into :a (Cyclotomic8 :a))
                    => (Into (Root2plex :a) (Cyclotomic8 :a)))
    (define (into a)
      (+ (into (root2-real-part a))
         ;; (e^(iπ/4)) - (e^(3iπ/4)) = √2
         (* (into (root2-root-part a)) (Cyclotomic8 -1 0 1 0)))))

  (define-instance ((Num :a) (Into :a (Cyclotomic8 :a)) (Complex :a)
                    => (Into (Complex :a) (Cyclotomic8 :a)))
    (define (into a)
      (+ (into (real-part a))
         ;; (e^(2iπ/4)) = i
         (* (into (imag-part a)) (Cyclotomic8 0 1 0 0)))))

  (declare cyclotomic8->complex
           ((Rational :a) (Elementary :b)
            => (Cyclotomic8 :a) -> (Complex :b)))
  (define (cyclotomic8->complex y)
    ;; HACK: Big-float memory corruption of pi on certain optimzation levels
    (let pi = (* 2 (asin 1)))
    (match (map
            (fn (x)
              (fraction->reciprocable (to-fraction x)))
            y)
      ((Cyclotomic8 a b c d)
       (+ (+ (s* a (cis (/ (* 3 pi) 4)))
             (s* b (cis (/ pi 2))))
          (+ (s* c (cis (/ pi 4)))
             (complex d 0))))))

  (define-instance ((Complex :a)
                    => (Into (Root2plex (Complex :a)) (Cyclotomic8 :a)))
    (define (into a)
      (Cyclotomic8
       ;;    (e^(iπ/4)) + (e^(3iπ/4)) = i√2
       (- (imag-part (root2-root-part a))
          ;; (e^(iπ/4)) - (e^(3iπ/4)) = √2
          (real-part (root2-root-part a)))
       ;; (e^(2iπ/4)) = i
       (imag-part (root2-real-part a))
       ;; (e^(iπ/4)) + (e^(3iπ/4)) = i√2
       (+ (imag-part (root2-root-part a))
          ;; (e^(iπ/4)) - (e^(3iπ/4)) = √2
          (real-part (root2-root-part a)))
       (real-part (root2-real-part a)))))

  (define-instance ((Complex :a)
                    => (Into (Complex (Root2plex :a)) (Cyclotomic8 :a)))
    (define (into a)
      ;; a + b√2 + (c + d√2)i
      (Cyclotomic8
       ;; b - d
       (- (root2-root-part (imag-part a))
          (root2-root-part (real-part a)))
       ;; c
       (root2-real-part (imag-part a))
       ;; b + d
       (+ (root2-root-part (imag-part a))
          (root2-root-part (real-part a)))
       ;; a
       (root2-real-part (real-part a)))))

  (define-instance ((Num :a) => (ExactRoot2 (Cyclotomic8 :a)))
    (define (root2-conjugate p)
      "Maps aω³+bω²+cω+d to -aω³+bω²-cω+d"
      (match p
        ((Cyclotomic8 a b c d) (Cyclotomic8 (negate a) b (negate c) d)))))

  (define-instance ((Num :a) => (Dagger (Cyclotomic8 :a)))
    (define (dagger p)
      "Maps aω³+bω²+cω+d to -cω³-bω²-aω+d"
      (match p
        ((Cyclotomic8 a b c d)
         (Cyclotomic8 (negate c) (negate b) (negate a) d)))))

  (define-instance (Factorable (Cyclotomic8 Integer))
    (define (euclid-norm a) (cyclotomic8-norm a))
    (define (canonical-factor x)
      (let ordered =
        (match x
          ((Cyclotomic8 a b c d)
           (let abs-a = (abs a))
           (let abs-b = (abs b))
           (let abs-c = (abs c))
           (let abs-d = (abs d))
           (cond
             ((and (and (> abs-a abs-b) (> abs-a abs-c))
                   (> abs-a abs-d))
              (Cyclotomic8 b c d (negate a)))
             ((and (> abs-b abs-c) (> abs-b abs-d))
              (Cyclotomic8 c d (negate a) (negate b)))
             ((> abs-c abs-d)
              (Cyclotomic8 (negate d) a b c))
             (True x)))))
      (if (> 0 (omega-empty-term ordered))
          (negate ordered)
          ordered))
    (define (euclid-div x y)
      ;; N(y) = σ(yy*)yy*
      ;; x / y = σ(yy*)xy*/(N(y))
      (let dividend =
        (* (* x (dagger y)) (root2-conjugate (* y (dagger y)))))
      (let divisor = (euclid-norm y))
      (match dividend
        ((Cyclotomic8 a b c d)
         (Cyclotomic8 (euclid-div a divisor)
                      (euclid-div b divisor)
                      (euclid-div c divisor)
                      (euclid-div d divisor)))))
    (define (euclid-gcd a b)
      (general-gcd a b))))
