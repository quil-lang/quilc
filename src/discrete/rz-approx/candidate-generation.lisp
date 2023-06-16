;;;; src/discrete/rz-approx/candidate-generation.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete/rz-approx)

(named-readtables:in-readtable coalton:coalton)

;;; Implements the candidate generation portion of the algirithm described in
;;; arXiv:1212.6253v2. Note that a different candidate generation technique can
;;; be implemented using arXiv:1403.2975.

(coalton-toplevel

  (declare encapsulating-half-open-unit-interval
           ((Root2plex Fraction) -> Integer))
  (define (encapsulating-half-open-unit-interval a)
    "Given a point A/B = x∈ℝ, find c∈ℤ s.t. c-1≤x<c. See Eq (20) and (21)
in (arXiv:1212.6253v2)"
    ;; if x = ⌊x⌋ = ⌈x⌉ then c ≄ x and c-1 = x = ⌊x⌋
    ;; if x ≄ ⌊x⌋ ≄ ⌈x⌉ then c = ⌈x⌉ = ⌊x⌋+1 and c-1 = ⌊x⌋
    ;; c-1 ≤ x < c ⇔ c = ⌊x⌋ + 1
    (+ 1 (floor a)))

  (declare lambda-scale
           ((Root2plex Fraction) -> (Root2plex Fraction) ->
            (Root2plex Fraction) -> (Root2plex Fraction) ->
            Integer -> (Root2plex Integer)))
  (define (lambda-scale x y delta-x delta-y n)
    "Given an X=x, Y=y, DELTA-X=δ, DELTA-Y=Δ, and an integer N=n, return an
 α∈ℤ[√2], such that (α, σ(α)) ∈ [x, x + δ] ⨉ [y, y + Δ], where λⁿβ = α such
that (β, σ(β)) ∈ [λⁿx, λⁿx + λⁿδ] ⨉ [λ⁻ⁿy, λ⁻ⁿy + λ⁻ⁿΔ]."
    ;; λ = 1 + √2
    (let lambda = (the (Root2plex Fraction) (Root2plex 1 1)))
    (let lambda-n = (^^ lambda n))
    ;; (σ λⁿ)
    (let lambda-n-conj = (root2-conjugate lambda-n))

    ;; (σ λ) = -1/λ
    ;; σ(λⁿ) = (-1)ⁿ λ⁻ⁿ
    (let lambda-n-recip = (if (odd? n)
                              ;; σ(λⁿ) = -λ⁻ⁿ ; n is odd
                              (negate (root2-conjugate lambda-n))
                              ;; σ(λⁿ) = λ⁻ⁿ ; n is even
                              (root2-conjugate lambda-n)))

    ;; (α, σ(α)) ⇔ α = λ⁻ⁿβ ⇔ (λ⁻ⁿβ, σ(λ⁻ⁿ)σ(β)) = (λ⁻ⁿβ, ±λⁿσ(β))
    (* (map round lambda-n-recip)
       ;; When n is even
       ;; (β, σ(β)) ∈ [λⁿx, λⁿx + λⁿδ] ⨉ [λ⁻ⁿy, λ⁻ⁿy + λ⁻ⁿΔ]
       ;;           = [λⁿx, λⁿx + λⁿδ] ⨉ [σ(λⁿ)y, σ(λⁿ)y + σ(λⁿ)Δ]
       ;; When n is odd
       ;; (β, -σ(β)) ∈ [λⁿx, λⁿx + λⁿδ] ⨉ [-λ⁻ⁿ(+ y Δ), -λ⁻ⁿy]
       ;;            = [λⁿx, λⁿx + λⁿδ] ⨉ [σ(λⁿ)(+ y Δ), σ(λⁿ)y]
       (coverage-solution-in-zroot2
        (* lambda-n x)
        (* lambda-n-conj (if (odd? n) (+ delta-y y) y))
        (* lambda-n delta-x)
        ;; 0 < λ⁻ⁿ = ±σ(λⁿ)
        (* lambda-n-recip delta-y))))

  (declare
   coverage-solution-in-zroot2
   ((Root2plex Fraction) -> (Root2plex Fraction) ->
    (Root2plex Fraction) -> (Root2plex Fraction) ->
    (Root2plex Integer)))
  (define (coverage-solution-in-zroot2 x y delta-x delta-y)
    "Given \"arbitrary\" reals X and Y and and interval lengths
DELTA-X = x₁-x₀ = δ and DELTA-Y = y₁-y₀ = Δ where [x₀ , x₁] and [y₀, y₁] are
intervals. If Δδ ≥ (1 + √2)², the returned element of a + b √2 = α ∈ ℤ[√2]
will satisify, a + b √2 ∈ [x, x + δ] and a - b √2 ∈ [y, y + Δ]. This function
corresponds to Lemma 17 (arXiv:1212.6253v2)."
    ;; λ = 1 + √2
    (let lambda = (the (Root2plex Fraction) (Root2plex 1 1)))

    (cond
      ((and (>= delta-x lambda) (>= delta-y root2))
       (progn

         ;; a-1 ≤ (x+y+Δ)/2 < a
         (let a = (encapsulating-half-open-unit-interval
                   (/ (+ (+ x y) delta-y) 2)))

         ;; (b-1)√2  ≤ (x-y-Δ)/2 < b√2
         (let b = (encapsulating-half-open-unit-interval
                   ;; Or (b-1) ≤ (x-y-Δ)*2^(-3/2) < b
                   (/ (- (- x y) delta-y) (* 2 root2))))

         (let alpha0 = (Root2plex a b))
         (let alpha1 = (Root2plex a (+ b 1)))
         (let alpha2 = (Root2plex (- a 1) b))
         (let alpha0-conj = (root2-conjugate alpha0))

         (cond
           ((<= (map fromInt alpha0-conj) (+ y delta-y))
            alpha0)
           ((<= (map fromInt alpha0) (+ x 1))
            alpha1)
           (True
            alpha2))))
      ((and (>= delta-x 1) (>= delta-y (+ 2 root2)))
       ;; (λδ,λ⁻¹Δ)
       (lambda-scale x y delta-x delta-y 1))
      ;; Commute the cases
      ((and (>= delta-x root2) (>= delta-y lambda))
       (root2-conjugate (coverage-solution-in-zroot2 y x delta-y delta-x)))
      ((and (>= delta-x (+ 2 root2)) (>= delta-y 1))
       (root2-conjugate (coverage-solution-in-zroot2 y x delta-y delta-x)))
      ;; Δδ ≥ (1 + √2)² and Δ,δ > 0
      ((and (and (> delta-x 0) (> delta-y 0))
            (>= (* delta-x delta-y) (^ lambda 2)))
       (let
           ((check (fn (x) (and (< 1 x) (<= x lambda))))
            (find-n
              (fn (n)
                "Find an n such that 1<δλⁿ≤λ"
                (if (check (* delta-x (^^ lambda n)))
                    (the Integer n)
                    (find-n (if (<= n 0) (+ 1 (negate n)) (negate n))))))
            (n (the Integer (find-n 0))))
         ;; (λⁿδ,λ⁻ⁿΔ)
         (lambda-scale x y delta-x delta-y n)))
      (True
       (error "Conditions not met in `interval-solution-in-zroot2'"))))

  (declare interval-solution
           ((Interval (Root2plex Fraction)) ->
            (Interval (Root2plex Fraction)) -> (Root2plex Integer)))
  (define (interval-solution x-set y-set)
    "Given intervals X-SET = [x₀ , x₁] and Y-SET = [y₀, y₁], let δ = x₁-x₀ and
Δ = y₁-y₀. If Δδ ≥ (1 + √2)², the returned element of a + b √2 = α ∈ ℤ[√2] will
satisify, a + b √2 ∈ [x₀ , x₁] and a - b √2 ∈ [y₀, y₁]. This function
corresponds to Lemma 17 (arXiv:1212.6253v2)."
    (coverage-solution-in-zroot2 (lower x-set)
                                 (lower y-set)
                                 (interval-distance x-set)
                                 (interval-distance y-set)))

  (declare rescaled-interval-solution
           (Boolean
            -> (Interval (Root2plex Fraction))
            -> (Interval (Root2plex Fraction))
            -> (Root2plex Integer)))
  (define (rescaled-interval-solution odd? x-set y-set)
    "Given intervals X-SET = [x₀ , x₁] and Y-SET = [y₀, y₁], let δ = x₁-x₀ and
 Δ = y₁-y₀. If Δδ ≥ 2(1 + √2)², the returned element of a + b √2 = α ∈ ℤ[√2]
 will satisify, a + b √2 ∈ [x, x + δ] and a - b √2 ∈ [y, y + Δ] where a is
odd/even depending on ODD? being True/False resp. This function corresponds
to Corrolary 19 (arXiv:1212.6253v2)."
    (if (not odd?)
        ;; Base case finds an a + b√2 with an even a-term
        (match (interval-solution
                ;; [x_0 / √2, x_1 / √2]
                (*s x-set (Root2plex 0 (exact/ 1 2)))
                ;; [-y_1 / √2, -y_0 / √2]
                (s* (the (Root2plex Fraction) -1)
                    (*s y-set (Root2plex 0 (exact/ 1 2)))))
          ;; Give a + b √2
          ;; Return (2 * b) + a √2
          ((Root2plex a b) (Root2plex (* 2 b) a)))

        ;; Otherwise finds an a + b√2 with an odd a-term
        (match (rescaled-interval-solution
                ;; Recurse the opposite case
                False
                ;; [x_0 - 1, x_1 - 1]
                (- x-set (Interval 1 1))
                ;; [y_0 - 1, y_1 - 1]
                (- y-set (Interval 1 1)))
          ;; Give a + b√2
          ;; Return (a + 1) + b √2
          ((Root2plex a b) (Root2plex (+ a 1) b)))))

  (define-type (Line :a)
    (Line :a :a))

  (declare slope ((Num :a) => ((Line :a) -> :a)))
  (define (slope l)
    (match l
      ((Line m _) m)))

  (declare intercept ((Num :a) => ((Line :a) -> :a)))
  (define (intercept l)
    (match l
      ((Line _ b) b)))

  (declare line-from-points ((Reciprocable :a) =>
                             (:a -> :a -> :a -> :a -> (Result :a (Line :a)))))
  (define (line-from-points ax ay bx by)
    "Given two points (AX, AY) and (BX, BY), construct a line equation that
 passes through them given by y = mx+b and return a (Ok Line :a) with slope m
 and intercept b or (Err x-intercept :a) if the slope is undefined (i.e. a
 vertical line)."
    ;; let a = xa + ya i and b = xb + yb
    (if (== ax bx)
        (Err ax)
        (progn
          (let delta-x = (- ax bx))
          (let delta-y = (- ay by))
          ;; Δy / Δx
          (let slope = (/ delta-y delta-x))
          ;; y = mx + b
          ;; b = y - mx
          (let i-intercept = (- ay (* ax slope)))
          (Ok (Line slope i-intercept)))))

  (declare parallelogram-segment ((Reciprocable :a) =>
                                  ((Result :a (Line :a)) -> :a -> :a -> :a)))
  (define (parallelogram-segment lin width y)
    "Construct a parallelogram parallel to a LIN with a WIDTH. Then for a given
 horizontal line of height Y return the offset by width intercepting x value
 with the line."
    (match lin
      ((Ok lin)
       (progn
         (let m = (slope lin))
         (let b = (intercept lin))
         ;; y = mx + b ⇒ x = (y - b) / m
         (let x1 = (/ (- y b) m))
         (- x1 width)))
      ;; The line is vertical
      ;; The parallelogram is a rectangle
      ((Err x) (- x width))))

  (declare decompose-basis-rotation
           ((Inner :a :b) (Normed :a :b) (Elementary :b)
            => (:a -> :a -> :b -> (Tuple :a :a))))
  (define (decompose-basis-rotation u u-orth l)
    "Given two orthogonal unit vectors U and U-ORTH on the unit circle, shorten
the magnitude of U by a length L. Return the shorted vector and an orthogonal
vector whose sum with the shortened vector results is a counter-clockwise
rotation of the original vector U. That is:
    let u = û, u-orth = ŭ, and 〈û,ŭ〉 = 0
    u_a = (ǁuǁ - l) û,
    u_b = sin (arccos ǁu_aǁ) ŭ
    Which should mean, u_a² + u_b² = 1.
    Return (Tuple u_a u_b)"
    (let u-a = (- (norm u) l))
    ;; (sin (arccos x)) = √(1 - x²)
    (let u-b = (sqrt (- 1 (^ u-a 2))))
    (Tuple (s* u-a u) (s* u-b u-orth)))

  (declare j-sub-interval (Fraction -> (Interval Fraction) -> Integer
                                    -> (Interval Fraction)))
  (define (j-sub-interval l-rational y-interval)
    "Given an L-RATIONAL = ɛ²/4 and an interval Y-INTERVAL = [y_min, y_max],
construct the interval I_j = [y_j, y_j + ɛ²/8] for j ∈ {0 .. n-1}
where 1 < N ≤ ⌊4√2 /ɛ⌋. Note that y_(j + 1) - y_j = (y_max - y_min) / n > ɛ²/8."
    (let y-min = (lower y-interval))
    (let y-max = (upper y-interval))
    ;; l = ɛ²/4 => Δ = l/2 = ɛ²/8
    (let delta-y = (/ l-rational 2))
    (let interval-of-y = (fn (yj) (Interval yj (+ yj delta-y))))
    (fn (n)
      (the Integer n)
      (let y-of
        = (fn (j)
            (+ y-min (* (/ (fromInt j) (fromInt n)) (- y-max y-min)))))
      (interval-of-y (y-of (- n 1)))))

  (declare x-mapping (Complex :a => Boolean -> Complex :a -> :a))
  (define (x-mapping rotate?)
    (if rotate?
        real-part
        imag-part))

  (declare y-mapping (Complex :a => Boolean -> Complex :a -> :a))
  (define (y-mapping rotate?)
    (if rotate?
        imag-part
        real-part))

  (declare generate-u-candidate
           ((Elementary :a) => ((:a -> Fraction) -> Integer ->
                                :a -> :a -> Integer ->
                                (Complex (Root2plex Integer)))))
  (define (generate-u-candidate to-rational k epsilon theta)
    "For K ≥ C + log2(1/ɛ) with C = 5/2 + 2 log_2 (1 + √2), and EPSILON > 0,
return the 1 < Nth ≤ ⌊4√2 /ɛ⌋ candidate for the top-left element of an unitary
operator's matrix that ɛ-approximates a gate R_z(THETA). The conditions satisify
Lemma 17 and this function corresponds to Theorem 22 in (arXiv:1212.6253v2)."
    ;; The following code utilizes the fact that ℂ forms a vector space
    ;; And mixes notions with the corresponding ℝ² vectors
    ;; Furthermore, depending on θ we choose between the maps
    ;; Let z = a + b i ↦ (a, b), (b, a) = (x, y)
    ;; Let z = exp (-iθ/2 ) =  cos(θ/2) - i sin(θ/2)
    (let angle = (/ theta 2))
    (let z = (complex (cos angle) (negate (sin angle))))
    ;; Let iz = -sin θ + i cos θ = cos (θ + π/2) + i sin (θ + π/2)
    ;; where iz can be visualized as a 90° rotation of z
    ;; In this case z = ẑ and iz = ž
    ;; So {z,zi} = {ẑ,ž} are basis vectors
    (let iz = (* (complex 0 1) z))
    ;; l = ɛ²/4
    (let l = (/ (^ epsilon 2) 4))

    ;; Here we will choose to assign the x-axis to
    ;; whichever component of the complex plane
    ;; z is the closest too.
    ;; This avoids us from constructing candidate regions with 0-height
    ;; and is without a loss of generality
    ;; This resolves the problem of when
    ;; a + bi ↦ (a, b) but θ=π/2 so y_min - y_max = 0
    (let rotate? = (> (abs (to-rational (real-part z)))
                      (to-rational (same-type epsilon (^^ (sqrt 2) -1)))))

    ;; Let φ = arccos (cos θ) = arcsin (sin θ)
    ;; If π/4 < φ < 3π/4 or 5π/4 < φ < 7π/4
    ;; a + bi ↦ (b, a)
    ;; Else
    ;; a + bi ↦ (a, b)
    (let x = (to-rational (x-mapping rotate? z)))
    ;; (let y = (to-rational (y-mapping rotate? z)))

    ;; As `coverage-solutions-in-zroot2' requires positive inputs,
    ;; we will need to adjust accordingly
    (let negate-x? = (< x 0))

    ;; cos θ = (ɛ²/4) / Δx
    ;; Δx = (ɛ²/4) / cos θ = x_1 - x_0
    (let l-rational = (to-rational l))
    (let delta-x = (the (Root2plex Fraction) (into (/ l-rational x))))

    ;; z1 = (|z| - ɛ²/4) ẑ
    ;; z1' = (sin (arccos |z_1|)) ž
    (let decomposition = (decompose-basis-rotation z iz l))
    (let z-1 = (fst decomposition))
    (let z-1-prime = (snd decomposition))

    (let z-2 = (+ z-1 z-1-prime))
    (let z-3 = (- z-1 z-1-prime))
    (let z-2-y = (to-rational (y-mapping rotate? z-2)))
    (let z-3-y = (to-rational (y-mapping rotate? z-3)))

    ;; Note that proj_y corresponds to either Im or Re
    ;; Also if y is negated thenthe min and max are flipped
    ;; θ=0 ⇒  y_max (z1 + z1') · ŷ
    (let y-max = (max z-2-y z-3-y))
    ;; θ=0 ⇒ y_min (z1 - z1') · ŷ
    (let y-min = (min z-2-y z-3-y))

    ;; All hope is lost if the epsilon region has zero height
    (when (== y-max y-min)
      (error "Not enough numeric precision for given epsilon."))

    ;; Just some re-usable coefficients
    (let scale = (the (Root2plex Fraction) (^^ root2 k)))
    (let conj-scale = (the (Root2plex Fraction) (^^ root2 (- k 1))))

    ;; β˙ = [-√2^(k-1), √2^(k-1)]
    (let conj-interval = (Interval (negate conj-scale) conj-scale))
    ;; Construct the line given by z2 and z3 (and z)
    (let lin =
      (the (Result (Root2plex Fraction) (Line (Root2plex Fraction)))
           (line-from-points (into (to-rational (x-mapping rotate? z-2)))
                             (into z-2-y)
                             (into (to-rational (x-mapping rotate? z-3)))
                             (into z-3-y))))

    (fn (n)
      ;; I_j = [y_j, y_j + ɛ²/8] for j ∈ {0 .. n-1}
      ;; where y_j - y_min + j/n (y_max - y_min)
      (let sub-interval = (j-sub-interval l-rational (Interval y-min y-max) n))

      ;; β ∈ [√2^k y_n, √2^k(y_j + ɛ²/8)]
      (let beta-interval =
        (match sub-interval
          ((Interval a b)
           (Interval (* scale (into a))
                     (* scale (into b))))))

      ;; For each section find a valid beta
      (let beta = (interval-solution beta-interval conj-interval))

      (let beta-hat =
        (the (Root2plex Fraction) (/ (map fromInt beta) scale)))

      (let x0 = (parallelogram-segment lin delta-x beta-hat))
      (let alpha-interval =
        (if negate-x?
            (s* scale (Interval (- x0 (into l-rational)) x0))
            (s* scale (Interval x0 (+ x0 (into l-rational))))))
      (let a-odd? = (odd? (root2-real-part beta)))
      (let alpha = (rescaled-interval-solution
                    (not a-odd?) alpha-interval conj-interval))

      (let solution = (complex alpha beta))
      (complex (x-mapping rotate? solution) (y-mapping rotate? solution))))

  ;; Checks for the proof by construction
  (declare in-ball? ((Ord :f) (Num :f) (Inner (:v :f) :f)
                     => (:v :f) -> :f -> Boolean))
  (define (in-ball? v r)
    "Checks if a vector is in the closed unit disk, ball, etc."
    (<= (<.> v v) r))

  (declare in-epsilon-region?
           ((Inner (:v :f) :f) (Ord :f) (Reciprocable :f)
            => (:v :f) -> (:v :f) -> :f -> Boolean))
  (define (in-epsilon-region? u z epsilon)
    (and (in-ball? u 1)
         (>= (abs (<.> u z)) (- 1 (/ (^ epsilon 2) 2))))))
