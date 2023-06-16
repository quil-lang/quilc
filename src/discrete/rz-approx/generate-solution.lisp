;;;; src/discrete/rz-approx/generate-solution.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/rz-approx)

(named-readtables:in-readtable coalton:coalton)

;;; Implements an algorithm for approximating RZ(θ) into a U made up of
;;; Clifford+T such that |RZ(θ) - U| is less than or equal to some ɛ. Currently
;;; utilizes arXiv:1212.6253v2, arXiv:1206.5236v4, and arXiv:1312.6584
;;; (see also arXiv:1403.2975, and arXiv:1212.0506).

(coalton-toplevel
  (declare until ((:a -> Boolean) -> (:a -> :a) -> :a -> :a))
  (define (until p f)
    (let ((next
            (fn (x)
              (if (p x) x
                  (next (f x))))))
      next))

  (declare find-some-until ((:a -> Boolean)
                            -> (:a -> :a)
                            -> (:a -> (Optional :b))
                            -> :a -> (Optional :b)))
  (define (find-some-until pred-x succ f x)
    "Searches for until (some? (F x)) or (PRED-X x)
where x = X, (SUCC X), (SUCC (SUCC X)), ... "
    (if (pred-x x)
        (progn
          (let current = (f x))
          (if (some? current)
              current
              (find-some-until pred-x succ f (succ x))))
        None))

  (declare print-operator-norm
           ((toFloat :a) =>
            UFix -> (SUnitary2 (Cyclotomic8 Dyadic)) -> :a -> :a -> Unit))
  (define (print-operator-norm precision solution epsilon theta)
    "Prints debug information about a given SOLUTION operator norm expcted with
Rz(THETA) to be less than or equal to EPSILON using PRECISION bits to calculate"
    (the UFix precision)
    (let thunk =
      (fn (_)
        (let theta = (toBig theta))
        (let epsilon = (toBig epsilon))
        (let m1 = (the (Mat2 (Complex :a)) (into (Rz theta))))
        (let m2 = (map cyclotomic8->complex
                       (the (Mat2 (Cyclotomic8 Dyadic))
                            (into solution))))
        (let d = (spectral-distance m1 m2))
        (let within = (<= d epsilon))
        (let percent = (abs (* 100 (best-approx (- (/ d epsilon) 1)))))
        (lisp Unit (within percent)
          (cl-quil.frontend:format-noise
           "Operator norm is ~:[greater~;less~] than epsilon by ~,2f%~%"
           within percent)
          Unit)))
    ;; We increase precision to verify our previous precision was enough
    (coalton-library/big-float:with-precision (+ 2 precision) thunk))

  (declare find-candidate
           ((Rational :a) (Reciprocable :a) (ToFloat :a)
            => (Integer -> :a -> :a ->  Integer
                        -> (Optional (SUnitary2 (Cyclotomic8 Dyadic))))))
  (define (find-candidate attempts epsilon theta)
    "For a given 1 < N ≤ ⌊4√2 /ɛ⌋, K = C + log2(1/ɛ) with C = 5/2 + 2 log_2 (1
+ √2), find a candidate for `generate-solution', and return a solution if it
 corresponds to an SU(2) matrix."
    (when (<= epsilon 0)
      (error "Precision must be positive."))
    (let epsilon-precision = (ceiling (ilog 2 (ceiling (reciprocal epsilon)))))
    (let precision = (the UFix (fromInt (* 2 (+ 8 epsilon-precision)))))

    ;; This value is rounded so a double-float should work
    (let c = (the Double-Float (+ (/ 5 2) (* 2 (log 2 (+ 1 (sqrt 2)))))))
    (let k = (ceiling (+ c (* 2 (log 2 (^^ (toDouble epsilon) -1))))))

    (let next-u =
      (if (> epsilon (^^ 10 -6))
          (generate-u-candidate best-approx k (toDouble epsilon) (toDouble theta))
          (coalton-library/big-float::with-precision precision
            (fn (_)
              (generate-u-candidate to-fraction k (toBig epsilon) (toBig theta))))))
    (fn (n)
      (do
       (let u = (next-u n))
       (let xi = (- (^ (the (Root2plex Integer) 2) k) (square-norm u)))
        ;; (* √2^(-n))
        (let factor = (times-root2^^ (negate k)))

        (t <- (maybe-find-t-candidate 2 (+ 2 attempts) xi))
        ;; ξ = (tt^†)
        (if (== (cyclotomic8-square-modulus t) xi)
            (progn
              ;; u-hat = (* u √2^(-n))
              (let u-hat =
                (factor
                 (the (Cyclotomic8 Dyadic)
                      (map fromInt
                           (the (Cyclotomic8 Integer) (into u))))))
              ;; t-hat = (* t √2^(-n))
              (let t-hat = (factor
                            (the (Cyclotomic8 Dyadic) (map fromInt t))))
              (match (sunitary2 u-hat t-hat)
                ((Some s)
                 (when (lisp Boolean () (to-boolean cl-quil.frontend:*compiler-noise*))
                   (print-operator-norm precision s epsilon theta))
                 (Some s))
                ((None) None)))
            None))))

  (declare generate-solution
           ((Rational :a) (Elementary :a) (ToFloat :a)
            => (Integer -> :a -> :a
                        -> (Optional (SUnitary2 (Cyclotomic8 Dyadic))))))
  (define (generate-solution attempts epsilon theta)
    "Given THETA which represents an angle of a R_z(θ) gate, return an
ɛ-approximate U where ‖ U − R_z(θ) ‖ ≤ EPSILON and U ∈ SU(2). There is a
theoretical chance of failure, but increasing ATTEMPTS or reducing EPSILON will
increase odds of success. ATTEMPTS is how many times to check the converse of
the square root of Fermat's Little Theorem (see `prime-mod-solution')
(ATTEMPTS will work well with the values 1, 2, or 3). Corresponds to the first
half of Algorithm 23 (arXiv:1212.6253v2)."

    (let n-max = (floor (/ (* 4 (sqrt 2)) epsilon)))
    (let search =
      (find-some-until
       (fn (n) (>= n-max n))
       (+ 1)
       (find-candidate attempts epsilon theta)))
    (search 1))

  (declare generate-maform-output
           ((Rational :a) (Reciprocable :a) (ToFloat :a)
            => (Integer -> :a -> :a -> Integer -> (Optional MAForm))))
  (define (generate-maform-output attempts epsilon theta)
    "Facilitates `find-candidate' with various attempts at seeded values."
    (fn (n)
      (map
       (fn (x)
         (the MAForm
              (into
               (omega-unitary->ht-sequence
                (into x)))))
       (find-candidate attempts epsilon theta n))))

  ;; TODO: monomorphize was here but it won't compile as of Fri Jun 16 10:32:29 PDT 2023
  ;; (monomorphize)
  (declare generate-maform-output-with-double
           (Integer -> Integer -> Double-Float -> Double-float -> (Optional (List OutputGate1))))
  (define (generate-maform-output-with-double candidate-attempts prime-attempts epsilon theta)
    "If sucessful returns a list of H, S, and T gate in MAForm. It try
CANDIDATE-ATTEMPTS number of times, and will use PRIME-ATTEMPTS to determine
how many prime checks to run on an individual candidate before moving onto the next.
See `generate-solution' for information about EPSILON and THETA."
    ;; Memoizing on solutions can greatly speed up QUILC compilations
    ;; when it repeatidly queries for the same simplification
    (let candidates = (floor (/ (* 4 (sqrt 2)) epsilon)))

    ;; List of functions for finding candidates - this precomputes some values
    (let candidate-finders =
      (map
       (fn (seed)
         (cond
           ((== seed 0) (generate-maform-output prime-attempts epsilon theta))
           ;; At 10^-15 this trick introduces errors due to the precision of pi
           ((and (< seed 8) (> epsilon (^^ 10 -15)))
            ;; T^n Rz(theta - m/4 pi)
            (let f = (generate-maform-output
                      prime-attempts epsilon
                      (- theta (* pi (general/ seed 4)))))
            (fn (n)
              (@ (Some (into (make-list (HT-T^^ seed))))
                 (f n))))
           (True
            ;; Rz(beta) Rz(theta - beta)
            (let beta = (psuedo-random seed))
            ;; We need to convert to fractions for higher precision
            (let f = (generate-maform-output
                      prime-attempts  (/ (to-fraction epsilon) 2) beta))
            (let g = (generate-maform-output
                      prime-attempts (/ (to-fraction epsilon) 2)
                      (- (to-fraction theta) beta)))
            (fn (n) (@ (f n) (g n))))))
       (range 0 candidate-attempts)))
    ;; For n, go through each candidate-finder once then proceed to n+1
    ;; Stop on a solution
    (let ((rec (fn (finders n)
                 (match finders
                   ((Cons f fs)
                    (match (f n)
                      ((Some a) (Some a))
                      ((None) (rec fs n))))
                   ((Nil)
                    (if (< n candidates)
                        (rec candidate-finders (+ n 1))
                        None))))))
      (map into (rec candidate-finders 1))))

  (monomorphize)
  (declare output-T^ (Integer -> (List OutputGate1)))
  (define (output-T^ n)
    "Give a list of gates equal to T^n"
    (operators::universal1->outputgate1 (into (HT-T^^ n))))

  #+sbcl
  (declare output-distance-rz (Big-Float -> (List OutputGate1) -> Big-Float))
  (define (output-distance-rz theta ma)
    (let m1 = ((the ((Rz :a) -> (Mat2 (Complex :a))) into) (Rz theta)))
    (let m2 = (the (Mat2 (Cyclotomic8 Dyadic))
                   (into
                    (the (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
                         (into
                          (the MAForm
                               (into
                                (the (List Universal1)
                                     (map into ma)))))))))
    (same-type theta
               (global-phase-invariant-distance
                m1 (map cyclotomic8->complex m2))))



)
