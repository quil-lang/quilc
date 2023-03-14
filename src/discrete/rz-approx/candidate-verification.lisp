;;;; src/discrete/rz-approx/candidate-verification.lisp
;;;;
;;;; Author: A.J. Nyquist

;;; Implements the candidate verification portion of the algorithm described in
;;; arXiv:1212.6253v2 and arXiv:1403.2975. Uses primality check from arXiv:1212.6253v2,
;;; but one could alternativly implement the prime factorization method described in
;;; arXiv:1403.2975.

(in-package #:cl-quil.discrete/rz-approx)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare prime-mod-solution (Integer -> Integer -> Integer
                                       -> (Optional Integer)))
  (define (prime-mod-solution start end p)
    "For an integer P, find a solution to b^((p - 1)/2) ≡ 1 (mod p) with
START ≤ B ≤ END. If P is prime, b^((p - 1)/2) ≡ ±1 (mod p), so a solution
should be found on average with (START - END) = 2 (assuming Prob(-1) = Prob(1)).
Note, START=0,1 are non-solutions. Corresponds to Remark 11 (arXiv:1212.6253v2)."
    (let max-iteration = (abs (min p end)))
    (let ((prime-mod-rec
            (fn (b)
              (cond
                ((== (- p 1) ;; = (mod -1 p) for p > 1
                     (expt-mod b (floor/ (- p 1) 2) p))
                 (Some
                  (expt-mod b (floor/ (- p 1) 4) p)))
                ((>= b max-iteration)
                 None)
                (True (prime-mod-rec (+ b 1)))))))
      (prime-mod-rec start)))

  (declare square-root-of-root2-unit
           ((Root2plex Integer) -> (Optional (Root2plex Integer))))
  (define (square-root-of-root2-unit x)
    "Find the square root of a Root2plex X as an Optional only if X is a square
by iterating over units less than or equal to X. See Lemma 10, (arXiv:1212.6253v2)."
    ;; x = a + √2 b
    ;; d = a^2 - 2 b^2
    ;; r = ⌊|x|⌋
    (let d = (the Integer (square-norm x)))
    (let r = (the Integer (isqrt d)))
    (let a = (the Integer (root2-real-part x)))
    (let alpha = (Root2plex (isqrt (div (+ a r) 2))
                            (isqrt (div (- a r) 4))))
    (let beta = (Root2plex (isqrt (div (- a r) 2))
                           (isqrt (div (+ a r) 4))))

    (cond
      ((== x (^ alpha 2)) (Some (abs alpha)))
      ((== x (^ beta 2)) (Some (abs beta)))
      ((== x (^ (root2-conjugate alpha) 2))
       (Some (abs (root2-conjugate alpha))))
      ((== x (^ (root2-conjugate beta) 2))
       (Some (abs (root2-conjugate beta))))
      (True None)))

  (declare maybe-find-t-candidate
           (Integer -> Integer -> (Root2plex Integer)
                    -> (Optional (Cyclotomic8 Integer))))
  (define (maybe-find-t-candidate start end xi)
    "Given an ξ = a + b√2, where a is odd, b is even, ξ > 0, σ(ξ) > 0,
and p=σ(ξ)ξ=x²−2y² is prime in ℤ. Then there exists t∈ℤ[ω] satisfying tt^† = ξ.
However, our given ξ (XI) may or may not satisfy the prime condition, but that
is okay as we will just check the a posteriori conditions which are of more
interest to us. See `prime-mod-solution' for START and END are passed to it.
Corresponds to Theorem 12 (arXiv:1212.6253v2)."
    (when (or (< (root2-conjugate xi) 0) (< xi 0))
      (error "Did not compute a valid candidate."))
    (do
     (h <- (prime-mod-solution start end (square-norm xi)))
     (let a = (the (Cyclotomic8 Integer) (into xi)))
      (let b = (into (complex (Root2plex h 0) 1)))
      ;; s = gcd(h + i, ξ)
      (let s = (euclid-gcd a b))
      (let u = (the (Root2plex Fraction)
                    ;; ξ / (ss^†)
                    (general/ xi (cyclotomic8-square-modulus s))))
      (u-int <- (match u
                  ((Root2plex a b)
                   (if (and (== 1 (denominator a))
                            (== 1 (denominator b)))
                       (Some (Root2plex
                              (numerator a)
                              (numerator b)))
                       None))))
      (v <- (square-root-of-root2-unit u-int))
      (pure (* (into v) s)))))
