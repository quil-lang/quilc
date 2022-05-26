;;;; tests/discrete/rz-approx-tests.lisp
;;;;
;;;; Author: A.J. Nyquist

(cl:in-package #:cl-quil.discrete-coalton-tests)

;;; Tests solver found in src/discrete/rz-approx/

(coalton-toplevel

  (define (test-interval-solution x-set y-set)
    (let solution =
      (rz::interval-solution x-set y-set))
    (let solution-commute =
      (rz::interval-solution y-set x-set))

    (is (interval-elem (map fromInt solution) x-set))

    (is (interval-elem (map fromInt solution-commute) y-set))

    (is (interval-elem (map fromInt (root2-conjugate solution)) y-set))
    (is (interval-elem (map fromInt (root2-conjugate solution-commute)) x-set)))

  (define (test-rescaled-solution x-set y-set)
    (let delta-x = (interval-distance x-set))
    (let delta-y = (interval-distance y-set))
    (is (>= (* delta-x delta-y) (* 2 (^ (Root2plex 1 1) 2))))

    (let odd-solution =
      (rz::rescaled-interval-solution True x-set y-set))
    (let even-solution =
      (rz::rescaled-interval-solution False x-set y-set))

    (is (odd? (root2-real-part odd-solution)))
    (is (even? (root2-real-part even-solution)))

    (is (interval-elem (map fromInt odd-solution) x-set))
    (is (interval-elem (map fromInt even-solution) x-set))

    (is (interval-elem (map fromInt (root2-conjugate even-solution)) y-set))
    (is (interval-elem (map fromInt (root2-conjugate odd-solution)) y-set))))

(define-test rescaled-interval-solution ()
  (let xy = (the (Root2plex Fraction)
                 (* 2 (Root2plex 1 1))))
  (let xy-set = (Interval 0 xy))
  (test-rescaled-solution xy-set xy-set)
  (test-interval-solution (Interval 0 (* (^ 10 9) xy)) (Interval (* xy (^^ -10 -9)) 0))
  (test-rescaled-solution (Interval -10 10) (Interval -2 2)))

(define-test interval-solution-in-zroot2 ()
  (let xy = (the (Root2plex Fraction)
                 (Root2plex 1 1)))
  (let zero = (Root2plex 0 (the Fraction 0)))
  (let root2 = (Root2plex 0 (the Fraction 1)))
  (let xy-set = (Interval 0 xy))
  (test-interval-solution xy-set xy-set)

  (test-interval-solution (Interval zero 10) (Interval -20 20))
  (test-interval-solution (Interval zero 100) (Interval 0 20))

  (test-interval-solution (Interval zero root2)
                          (Interval zero (+ 1 root2)))
  (test-interval-solution (Interval zero 1)
                          (Interval zero (+ 2 root2)))
  (test-interval-solution (Interval -1 root2)
                          (Interval (negate root2) zero))

  (test-interval-solution (Interval -11 -10)
                          (Interval zero (+ 2 root2)))
  (test-interval-solution (Interval zero root2)
                          (Interval -1 root2))

  (test-interval-solution (Interval -1 (- (* 10 (^ xy 2)) 1))
                          (Interval 1 (+ (Root2plex
                                          (exact/ 1 10) 0) 1)))

  (test-interval-solution (Interval 0 (* (^ 10 200) (^ xy 2)))
                          (Interval 0 (Root2plex
                                       (^^ 10 -200) 0)))

  (test-interval-solution (Interval (*. xy (best-approx -2.42))
                                    (*. root2 (best-approx 2.33)))
                          (Interval (*. root2 (best-approx -2.2))
                                    (*. xy (best-approx 2.4)))))

(define-test j-sub-intervals ()
  (let epsilon = 0.05)
  (let n-max = (floor (/ (* 4 (sqrt 2)) epsilon)))
  (let y-interval = (Interval (exact/ 1 4) (exact/ 1 2)))
  (let c = (/ (interval-distance y-interval) (fromInt n-max)))
  (is (> (into c) (/ (^ epsilon 2) 8)))

  (let sub-intervals =
    (map (rz::j-sub-interval (best-approx (/ (^ epsilon 2) 8))
                             y-interval)
         (list:range 1 n-max)))
  (map
   (fn (interval)
     (is (and (< (interval-distance interval) c)
              (> (interval-distance interval) 0))))
   sub-intervals)

  (is (== n-max (list:length sub-intervals))))

;;; Test candidate generation

(coalton-toplevel
  (declare valid-k-epsilon
           ((Elementary :a) (Ord :a) => (Integer -> :a -> Boolean)))
  (define (valid-k-epsilon k epsilon)
    ;; C = 5/2 + 2 log_2 (1 + √2)
    (let c = (+ (/ 5  2) (log 2 (+ 1 (sqrt 2)))))
    ;; k ≥ C + log 2(1/ɛ)
    (let min-k = (+ c (log 2 (^^ epsilon -1))))
    ;; Assert (30) δΔ = (1 + √2)² by Lemma 17 and min-k
    (and (and (> epsilon 0) (<= epsilon (/ 1 2)))
         (>= (fromInt k) min-k)))

  (declare verify-u-candidate ((Elementary :a) (Rational :a)
                               => Integer -> :a -> :a
                               -> (Complex (Root2Plex Integer))
                               -> Unit))
  (define (verify-u-candidate k epsilon theta u)
    "Checks if U is a valid candidate for a given K, EPSILON, and THETA.
Corresponds to Lemma 21 (Selinger, 2014)."
    (let alpha = (real-part u))
    (let beta  = (imag-part u))
    (let z = (exp (complex 0 (/ theta -2))))
    (let u-hat = (*. (complex (map (fn (x) (exact/ x 1)) alpha)
                              (map (fn (x) (exact/ x 1)) beta))
                     (^^ (the (Root2plex Fraction) root2) (negate k))))
    (let u-hat-float = (complex
                        (numeric::rroot2->floating (real-part u-hat))
                        (numeric::rroot2->floating (imag-part u-hat))))
    (is (rz::in-ball? (root2-conjugate u-hat) (the (Root2plex Fraction) 1)))
    (is (rz::in-ball? u-hat (the (Root2plex Fraction) 1)))
    (is (rz::in-epsilon-region? u-hat-float z epsilon))
    (is (odd? (+ (root2-real-part alpha)
                 (root2-real-part beta)))))

  (declare test-generate-u-candidates
           ((Rational :a) (Elementary :a) =>
            (:a -> :a -> Unit)))
  (define (test-generate-u-candidates epsilon theta)
    (let c = (+ (/ 5 2) (* 2 (log 2 (+ 1 (sqrt 2))))))
    (let k = (ceiling (+ c (* 2 (log 2 (^^ epsilon -1))))))

    (let epsilon-prime = (best-approx epsilon))
    (let n-max = (floor (/ (* 4 (sqrt 2)) epsilon)))
    (is (valid-k-epsilon k epsilon))

    ;; δ = √(2^k) ɛ²/8
    (let delta-x = (* (^ root2 k)
                      (Root2plex (/ (^ epsilon-prime 2) 8) 0)))
    ;; Δ = √(2^(k+1))
    (let delta-y = (+ (^ root2 (+ k 1))
                      (* 0 delta-x)))

    (test-interval-solution (Interval 0 delta-x) (Interval 0 delta-y))

    (let candidates =
      (map (rz::generate-u-candidate best-approx k epsilon theta)
           (list:range 1 n-max)))

    (map (verify-u-candidate k epsilon theta) candidates)
    (is (== n-max (list:length candidates)))))

(define-test generate-u-candidates ()
  (test-generate-u-candidates 0.5 0.0)
  ;; Works but takes a while, smaller epsilons result in GC error
  ;; (test-generate-u-candidates 0.0001 0.1)
  (test-generate-u-candidates 0.01 (* pi 0.25))
  (test-generate-u-candidates 0.25 (* pi 0.5))
  (test-generate-u-candidates 0.015 (* pi 1.9))
  (test-generate-u-candidates 0.5 (* pi 0.9))
  (test-generate-u-candidates 0.5d0 (* pi 1.0d0))
  (test-generate-u-candidates 0.5 (* pi 1.1)))

;;; Test candidate verification

(define-test prime-mod-solution ()
  (is (isSome (rz::prime-mod-solution 2 7 7)))
  (is (isNone (rz::prime-mod-solution 0 10 10)))
  (is (isSome (rz::prime-mod-solution 0 100 16843))))

(define-test square-root-of-root2-unit ()
  (is (== (Some (^ (Root2plex -1 1) 4))
          (rz::square-root-of-root2-unit (^ (Root2plex -1 1) 8))))
  (is (== (Some (^ (Root2plex -1 1) 3))
          (rz::square-root-of-root2-unit (^ (Root2plex -1 1) 6))))
  (is (/= (Some (^ (Root2plex -2 1) 2))
          (rz::square-root-of-root2-unit (^ (Root2plex -1 2) 4)))))

;;; Test exact synthesis

(coalton-toplevel
  (declare ma-normal-form-check
           ((Unitary2 RootUnity8 (Cyclotomic8 Dyadic)) -> Boolean))
  (define (ma-normal-form-check unitary)
    (let maform = (the MAForm (into unitary)))
    (is (projective-equivalence
         unitary
         (foldable->action
          (the (List OutputGate1) (into maform)))))
    (== unitary (into maform)))

  (declare hadamardt-synthesis-check ((List HadamardT) -> Boolean))
  (define (hadamardt-synthesis-check hts)
    (let unitary = (foldable->action hts))
    (is (== unitary (foldable->action
                     (operator::omega-unitary->ht-sequence unitary))))
    (ma-normal-form-check unitary)))

(define-test exact-synthesis ()
  (is (hadamardt-synthesis-check (make-list HT-H)))
  (is (hadamardt-synthesis-check (make-list (HT-T^^ 10))))
  (is (hadamardt-synthesis-check (map (fn (_) HT-H) (list:range 0 10))))
  (is (hadamardt-synthesis-check (make-list  (HT-T^^ 3) HT-H)))
  (is (hadamardt-synthesis-check (make-list HT-H (HT-T^^ 2) HT-H)))
  (is (hadamardt-synthesis-check
       (make-list HT-H (HT-T^^ 1) HT-H (HT-T^^ 2)
                  HT-H (HT-T^^ 3) HT-H (HT-T^^ 4))))
  (is (hadamardt-synthesis-check (make-list HT-H (HT-T^^ 0))))
  (is (hadamardt-synthesis-check
       (make-list HT-H HT-H (HT-T^^ 2) (HT-T^^ 0)))))

;;; Test final candidate result

(coalton-toplevel
  (define (omega-unitary2->mat2 a)
    (the (Mat2 (Complex :a))
         (map numeric::cyclotomic8->complex
              (the (Mat2 (Cyclotomic8 Dyadic))
                   (into a)))))

  (define (candidate-check attempts epsilon theta)
    (match (rz::generate-solution attempts epsilon theta)
      ((Some a)
       (and
        ;; No matter the operator the spectral radius l.e. to the norm
        (>= epsilon ((the ((Mat2 (Complex :a)) -> :a)
                          spectral-radius)
                     (- (omega-unitary2->mat2 a)
                        (into (Rz theta)))))
        ;; Check MAForm is equal to original unitary
        (ma-normal-form-check (into a))))
      ((None) False))))

(define-test candidate-solver ()
  ;; Solutions are not gaurenteed - values may need to be adjusted
  (is (candidate-check 2 0.5d0 1))
  (is (candidate-check 4 0.02d0 3))
  (is (candidate-check 4 0.01d0 6))
  (is (candidate-check 2 (* 1 (pow 10 -4)) (* pi 1.25d0)))
  (is (candidate-check 2 (* 3 (pow 10 -4)) (* pi 0.3d0)))
  (is (candidate-check 4 (* 5 (pow 10 -8)) (* pi 0.5d0)))
  (is (candidate-check 4 0.25d0 (* pi 1.5d0))))

(coalton-toplevel
  (declare solution-check
           (Double-Float -> Double-Float -> (List OutputGate1) -> Boolean))
  (define (solution-check theta epsilon outputgates)
    (>= epsilon
        (global-phase-invariant-distance
         (omega-unitary2->mat2
          (the (Unitary2 RootUnity8 (Cyclotomic8 Dyadic))
               (foldable->action outputgates)))
         (the (Mat2 (Complex Double-Float))
              (into (Rz theta))))))

  (define (solution-check-float theta epsilon)
    (solution-check
     theta epsilon
     (unwrap
      (generate-maform-output-with-double
       25 2 epsilon theta))))

  (define (solution-check-pi n)
    (solution-check
     (* (general/ n 4) pi) (^^ 10 -7)
     (the (List OutputGate1) (output-T^ n)))))

(define-test discretize-rz-helpers ()
  (map (fn (n) (is (solution-check-pi n)))
       (range 0 16))
  (map (fn (n) (is (solution-check-float
                    (* (* 2 pi) (sin (fromInt n)))
                    (^^ (fromInt n) -2))))
       (range 2 30))
  Unit)
