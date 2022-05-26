;;;; src/discrete/numeric/linear-algebra.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete/numeric)

;;; Common classes, types, and functions for (linear) algebra code (e.g. group
;;; actions, modules/vectors, matrices, (linear) transformations, eigenvalues).

(coalton-toplevel

  (define-class (Scalar :v :s)
    "The scalar :S associated with :V where `zero-scalar' returns 0."
    (zero-scalar (:v -> :s)))

  (define-instance ((Num :s) => (Scalar (:v :s) :s))
    (define (zero-scalar _) 0))

  (define-class (Degreed :a)
    "Anything whose dimensions can be mapped from a constant N has degree N.
For example an N x N matrix has degree N."
    (degree (:a -> Integer)))

  (define-class ((Scalar :v :s) => (Linear :v :s))
    "Defines scalar multiplication where (.* x (+ v u)) = (+ (.* x u) (.* x v))"
    (.* (:s -> :v -> :v)))

  (declare *. ((Linear :v :e) (Num :e) => (:v -> :e -> :v)))
  (define (*. v s) (.* s v))

  (declare ./ ((Linear :v :e) (Reciprocable :e)
               => (:e -> :v -> :v)))
  (define (./ s v) (.* (/ 1 s) v))

  (declare /. ((Linear :v :e) (Reciprocable :e)
               => (:v -> :e  -> :v)))
  (define (/. v s) (./ s v))

  (define-class ((Linear :v :e) => (Inner :v :e))
    "An (indefinite) inner product that should satisfy:
    - (<.> x y) = (<.> y x)
    - (<.> z (+ (.* a x) (.* b y))) = (+ (*. (<.> z x) a) (*. (<.> z y) b)) "
    (<.> (:v -> :v -> :e)))

  ;; Note that normed spaces are a superset of definite inner product spaces.
  (define-class ((Linear :v :e) => (Normed :v :e))
    "A (indefinite) normed space that should satisify
    - (norm (+ x y)) <= (+ (norm x) (norm y))
    - (norm (.* s x)) = (* |s| (norm x))"
    (norm (:v -> :e)))

  (declare square-norm ((Inner :v :e) => (:v -> :e)))
  (define (square-norm v)
    "The canonical (indefinite) norm squared of a given inner product space for
an element V (i.e. ǁvǁ² = 〈v,v〉). For Euclidean vectors this is the magnitude
squared."
    (<.> v v))

  (declare canonical-norm ((Inner :v :e) (Elementary :e) => :v -> :e))
  (define canonical-norm
    "The canonical norm of a given inner product space for an element V (i.e.
ǁvǁ = √〈v,v〉). For Euclidean vectors this is the magnitude. This function may
be partial as there is no way to determine if an Inner (product) is
positive-definite. Use `norm' for positive-definite cases."
    (compose sqrt square-norm))

  ;;; Matrices and Operators

  (define-class ((Applicative :m) => (Matrix :m))
    "A matrix; or something with a matrix representation."
    (rows ((:m :e) -> Integer))
    (cols ((:m :e) -> Integer))
    (entry ((:m :e) -> Integer -> Integer -> :e))
    (map-over-indices ((:a -> Integer -> Integer -> :b)
                       -> (:m :a) -> (:m :b))))

  (define-class (Composable :f :g :h)
    "Generalizes composition or application where (apply f g) = fg = f∘g, f(g).
For example if apply is matrix multiplciation, g would be a matrix representing a
linear transformation V → U, and f may similarly be a U → V or a vector in V."
    ;; Can be thought of two ways
    ;;  - Convert :f into the function (:g -> :h)
    ;;  - Eval :f of :g to return :h
    (apply (:f -> (:g -> :h))))

  (define-class (Action :a :b)
    "An action is a closed `apply' operation."
    (act (:a -> :b -> :b)))

  (define-class ((Action :a :a) => Identity :a)
    "The Composable identity element."
    (identity :a))

  (define-class ((Identity :a) => (Inverse :a))
    "Satisfies (@ A (safe-inverse A)) = (Some identity) or None.
When safe-inverse is None, inverse may error."
    (inverse (:a -> :a))
    (safe-inverse (:a -> (Optional :a))))

  (define-instance ((Composable :f :x :x) => (Action :f :x))
    (define (act op v)
      ((the (:a -> :a) (apply op)) v)))

  ;; Ex: (coalton (act (+ 1) 1)) = 2
  (define-instance (Composable (Arrow :b :c) (Arrow :a :b) (Arrow :a :c))
    (define (apply f g x) (f (g x))))

  ;; Ex: (coalton (act (+ 1) (+ 1) 1)) = 3
  (define-instance (Composable (Arrow :a :b) :a :b)
    (define (apply f x) (f x)))

  ;; Overlapping instance bug
  ;; (define-instance (Identity (Arrow :a :a))
  ;;   (define identity (fn (x) x)))

  (declare constructor-act ((Action (:a :e) (:v :e)) => (:a :e) -> (:v :e) -> (:v :e)))
  (define (constructor-act a v)
    "Same as `act', but carries over constructed types."
    (act a v))

  ;; Ex: (coalton ((@ (+ 2) (+ 1)) -3)) = 0
  (declare @ ((Action :a :a) => :a -> :a -> :a))
  (define (@ op v)
    "Composition forming a semigroup."
    (act op v))

  ;; Ex: (coalton (@^ (+ 1) 3 1)) = 4
  (declare @^ ((Identity :f) => (:f -> Integer -> :f)))
  (define (@^ f n)
    (let ((act-rec
            (fn (x n)
              (if (> n 0)

                  (act-rec (act f x) (- n 1)) x))))
      (if (< n 0)
          (error "Can't apply negative times.")
          (act-rec identity n))))

  (declare @^^ ((Inverse :a) => (:a -> Integer -> :a)))
  (define (@^^ f n)
    (if (< n 0)
        (@^ (inverse f) (negate n))
        (@^ f n)))

  (declare foldable->action ((Into :a :b) (Identity :b) (Foldable :f) (Functor :f)
                             => ((:f :a) -> :b)))
  (define (foldable->action cts)
    "Convert foldable (e.g. a list) with elements as actions to an action."
    (fold @ identity (map into cts)))

  (define-instance ((Composable :a :b :c)
                    => (Composable (Optional :a) (Optional :b) (Optional :c)))
    (define apply (liftA2 apply)))

  (define-instance ((Identity :a) (Action (Optional :a) (Optional :a))
                    => (Identity (Optional :a)))
    (define identity (pure identity)))

  (declare square-transpose ((Matrix :m) => (:m :e) -> (:m :e)))
  (define (square-transpose m)
    (map-over-indices
     (fn (_ i j)
       (entry m j i))
     m))

  (define-class (Dagger :a)
    "Anything closed under a complex conjugate, conjugate transpose,
Hermitian adjoint, or related involution - often notated †."
    (dagger (:a -> :a)))

  (define-instance ((Num :a) (Dagger :a) (Complex :a)
                    => (Dagger (Complex :a)))
    (define (dagger a)
      "Standard complex conjugate: a + b i to a - b i"
      ;; We dagger the argument in the case of nested complex structures
      (complex (dagger (real-part a))
               (dagger (negate (imag-part a))))))

  (define-instance (Dagger Integer) (define (dagger a) (id a)))
  (define-instance (Dagger Fraction) (define (dagger a) (id a)))
  (define-instance (Dagger Single-Float) (define (dagger a) (id a)))
  (define-instance (Dagger Double-Float) (define (dagger a) (id a)))
  #+sbcl (define-instance (Dagger Big-Float) (define (dagger x) x))

  (declare dagger-square-norm ((Dagger :a) (Num :a) => :a -> :a))
  (define (dagger-square-norm a)
    (* a (dagger a)))

  (define-class ((Scalar :m :e) => (Det :m :e))
    "Linear operator with a determinant."
    (det (:m -> :e)))

  (define-class ((Scalar :m :e) => (Tr :m :e))
    "Linear operator with a trace."
    (tr (:m -> :e)))

  (declare projective-equivalence
           ((Tr (:a :e) :e) (Action (:a :e) (:a :e))
            (Degreed (:a :e))
            (Dagger (:a :e)) (Num :e) (Dagger :e)
            => ((:a :e) -> (:a :e) -> Boolean)))
  (define (projective-equivalence m n)
    "Checks two matrices are in the same projective class."
    ;; sqrt(1 - |trace (m @ n^†)|/(degree m)) = 0
    ;; 1 - |trace (m @ n^†)|/(degree m) = 0
    ;; |trace (m @ n^†)| = (degree m)
    ;; norm(trace (m @ n^†)) = (degree m)
    ;; norm(trace (m @ n^†))^2 = (degree m)^2
    (== (+ (fromInt (^ (degree m) 2)) ((the ((:a :e) -> :e) zero-scalar) m))
        (dagger-square-norm (tr (@ m (dagger n))))))

  (declare spectral-radius
           ((Ord :b) (Elementary :b) (Eigen :a (Complex :b))
            => :a -> :b))
  (define (spectral-radius m)
    "Finds the spectral radius of a complex finite matrix."
    (match (coalton-library/list:maximum
            (map magnitude (eigenvalues m)))
      ((Some lambda) lambda)
      ;; Eigenvalue of empty matrix is 0
      ((None) 0)))

  (declare spectral-distance
           ((Ord :b) (Elementary :b) (Matrix :m)
            (Eigen (:m (Complex :b)) (Complex :b))
            => ((:m (Complex :b)) -> (:m (Complex :b)) -> :b)))
  (define (spectral-distance m n)
    (spectral-radius (liftA2 - m n)))

  (declare global-phase-invariant-distance
           ((Elementary :b) (Degreed :a) (Normed (Complex :b) :b)
            (Identity :a) (Dagger :a) (Tr :a (Complex :b))
            => :a -> :a -> :b))
  (define (global-phase-invariant-distance m n)
    "Distance between two unitaries up to a phase."
    (sqrt (- 1 (/ ((the ((Complex :e) -> :e) norm)
                   (the (Complex :a) (tr (@ m (dagger n)))))
                  (fromInt (degree m))))))

  (define-class ((Scalar :m :e) => (Eigen :m :e))
    "An operator with computable lists of eigenvalues."
    (eigenvalues (:m -> (List :e))))

  (define-class ((Action :m :v) (Eigen :m :e) => (EigenLinear :m :v :e))
    "An operator with computable lists of eigenvectors and eigenvalues."
    (eigenvectors (:m -> (List :v)))
    (eigenZip (:m -> (List (Tuple :e :v)))))

  (define (dimensions m)
    (Tuple (rows m) (cols m)))

  (define (same-size? a b)
    (== (dimensions a) (dimensions b)))

  (define (valid-entry? m r c)
    (and (and (< 0 r) (<= r (rows m)))
         (and (< 0 c) (<= c (cols m)))))

  (define (safe-entry m r c)
    (if (valid-entry? m r c)
        (Some (entry m r c))
        None))

  ;;;; Implmentations
  ;;; Complex numbers

  (define-instance ((Complex :e) (Num :e) (Num (Complex :e))
                    => (Linear (Complex :e) :e))
    (define (.* s v)
      (complex (* s (real-part v)) (* s (imag-part v)))))

  (define-instance ((Linear (Complex :e) :e) (Complex :e)
                    => (Inner (Complex :e) :e))
    (define (<.> a b)
      (+ (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))))

  (define-instance ((Inner (Complex :f) :f) (Elementary :f)
                    => (Normed (Complex :f) :f))
    (define (norm x) (canonical-norm x))))
