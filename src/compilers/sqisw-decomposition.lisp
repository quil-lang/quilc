;;;; sqisw-decomposition.lisp
;;;;
;;;; Author: Colin O'Keefe Following the algorithm outlined in Huang
;;;; et al, https://arxiv.org/pdf/2105.06074.pdf, Supplemental
;;;; Material, I.B.

(in-package #:cl-quil)

;; Some defintions for making sense of comments and docstrings:
;;
;; L(p,q,r) is the canonical representative of the class with
;; coordinates are p,q,r.  Specifically, Huang et al define this to be
;;
;; L(p,q,r) = exp(i[p,q,r]·Σ)  where Σ = [X⊗X,Y⊗Y,Z⊗Z]
;;
;; Let  CAN(a,b,c) be the standard quil CAN gate.
;; Let Coords(M) be the canonical coordinates (a,b,c) such that M = A·CAN(a,b,c)·B
;; Then Coords(L(a/2,b/2,c/2)) = (a,b,c). See the following numerical demonstration:
;;
;; (assert
;;  (loop repeat 10000
;;        for coords = (canonical-coords (random-unitary 4) 1 0)
;;        for repcoords = (canonical-coords
;;                         (apply 'canonical-representative-huang-et-al
;;                                (mapcar (lambda (x) (/ x 2)) coords))
;;                         1 0)
;;        always (every 'double= coords repcoords)))
;;
;; Hence L(a/2,b/2,c/2) = A·CAN(a,b,c)·B for some A,B ∈ SU(2)⊗SU(2)
;;
;; Let ~ denote "local equivalence".

;; A function I use while hacking

;; (defun canonical-coords (gate &rest params-and-qubits)
;;   "Pluck the parameters from the canonical gate found in the
;; decomposition of GATE."
;;   (let ((gate
;;           (etypecase gate
;;             (gate-application gate)
;;             (magicl:matrix/complex-double-float
;;              (apply #'anon-gate "ANON" gate params-and-qubits))
;;             (string
;;              (apply #'build-gate gate params-and-qubits)))))
;;     (mapcar #'constant-value
;;             (application-parameters
;;              (third (canonical-decomposition gate))))))

(defun interleaving-1-qubit-gates (x y z q0 q1)
  "x y and z are canonical parameters in the Weyl chamber satisfying
pi/4 >= x >= y >= |z|. Furthermore, this function assumes that the
class of gates indicated by x,y,z falls within the span of 2 SQISW
gates, which is equivalent to the additional condtion that x >= y + |z|.

q0 and q1 are qubit indices.

Returns a list of two members, L0 and L1, both lists of single-qubit
gate applications, acting on q0 and q1 respectively.  L0 is a list of
three rotations RZ(γ) RX(α) RZ(γ), and L1 is a list containing one
RX(β).

L(x,y,z) ~ SQiSW [ RZ(γ)RX(α)RZ(γ) ⊗ RX(β)] SQiSW

NOTE: the above is '~' not '='

Where L(x,y,z) is the canonical representative of the class with coordinates x,y, and z."
  (declare (type double-float x y z))
  (let* ((c (* (sin (+ x y (- z)))
               (sin (+ x (- y) z))
               (sin (+ (- x) (- y) (- z)))
               (sin (+ (- x) y z))))
         (cos2x (cos (* 2 x)))
         (cos2y (cos (* 2 y)))
         (cos2z (cos (* 2 z)))
         (cos-x-sq (* (cos x) (cos x)))
         (cos-z-sq (* (cos z) (cos z)))
         (sin-y-sq (* (sin y) (sin y)))
         (2-sqrt-c (* 2 (sqrt c)))
         (alpha (acos (+ cos2x (- cos2y) cos2z 2-sqrt-c)))
         (beta (acos (+ cos2x (- cos2y) cos2z (- 2-sqrt-c))))
         (gamma (acos (* (if (minusp z) -1 1) ; sign(z)
                         (sqrt (/ (* 4 cos-x-sq cos-z-sq sin-y-sq)
                                  (+ (* 4 cos-x-sq cos-z-sq sin-y-sq)
                                     (* cos2x cos2y cos2z))))))))
    (list
     (list (build-gate "RZ" (list gamma) q0)
           (build-gate "RX" (list alpha) q0 )
           (build-gate "RZ" (list gamma) q0))
     (list (build-gate "RX" (list beta) q1)))))

(define-compiler canonical-to-2-sqisw
    ((instr ("CAN" (x y z) q1 q0)
            :where (<= (abs z) (- x y)))) ; this will be true for ≅ 80% of all coordinates x y z
  (destructuring-bind
      (c0 c1) (interleaving-1-qubit-gates (/ x 2.0d0) (/ y 2.0d0) (/ z 2.0d0) q0 q1) ; L(v/2) ~ CAN(v)
    (destructuring-bind
        (e0 e1 can2 d0 d1)
        (canonical-decomposition
         (anon-gate "V" (make-matrix-from-quil
                         (append
                          (list (build-gate "SQISW" () q1 q0))
                          c0 c1
                          (list (build-gate "SQISW" () q1 q0))))
                    q1 10))
      (declare (ignore can2))
      (inst (anon-gate "E0t" (magicl:conjugate-transpose (gate-matrix e0)) q0))
      (inst (anon-gate "E1t" (magicl:conjugate-transpose (gate-matrix e1)) q1))
      (inst "SQISW" () q1 q0)
      (mapc #'inst c0)
      (mapc #'inst c1)
      (inst "SQISW" () q1 q0)
      (inst (anon-gate "D0t" (magicl:conjugate-transpose (gate-matrix d0)) q0))
      (inst (anon-gate "D1t" (magicl:conjugate-transpose (gate-matrix d1)) q1)))))


;; The following collects 1000 valid canonical coordinates as befits
;; canononical-decomposition's conventions. With each one it builds a
;; CAN gate, decomposes it with the canonical-to-2-sqisw compilre, and
;; checks that the matrices are in the same projective class. This
;; should true 100% of the time.

;; (loop
;;   for (a b c) = (list (random pi/2) (random pi/2) (random pi/2))
;;   for should-apply? =  (and (quil-canonical-p a b c)
;;                             (<= (abs c) (- a b)))
;;   for gate = (build-gate "CAN" (list a b c) 1 0)
;;   when should-apply?
;;     count 1 into samples
;;   when (and should-apply?
;;              (matrix-equals-dwim
;;               (gate-matrix gate)
;;               (make-matrix-from-quil (canonical-to-2-sqisw gate))))
;;     count 1 into matches
;;   until (= 1000 samples)
;;   finally (assert (= samples matches)))
;; 
;; (defun quil-canonical-p (x y z)
;;   (and (>= pi/2 x y (abs z))
;;        (>= pi (+ x y))))



(defun canonicalize-for-sqisw (x0 y0 z0 q0 q1)
  "x0 y0 and z0 are canonical parameters in the Weyl chamber satisfying
pi/4 >= x >= |z|.  q0 and q1 are qubit indices.

Returns a list of 9 values. (x y z A0 A1 B0 B1 C0 C1) with the
following interpretation.

The Ai,Bi,Ci for i ∈ {0,1} are lists of single qubit gate applications
acting on qi, listed in the order they are applied.  As a shorthand,
below I use Gi to mean the gate obtained by applying gates
in Gi.

If x0,y0,z0 are canonical paramters for an arbitrary 2 qubit gate,
then

   L(x0,y0,z0) =  [A0 ⊗ A1]·L(x,y,z)·[B0 ⊗ B1]·SQiSW·[C0 ⊗ C1]


Furthermore, the returned x,y,z, we have that L(x,y,z) is a gate in
the span of two SQiSW applications."
  (declare (type double-float x0 y0 z0))
  (let ((A0 (list))                     ; essentially, the identity
        (A1 (list))
        (B0 (list (build-gate "RY" (list -pi/2) q0)))
        (B1 (list (build-gate "RY" (list pi/2) q1)))
        (C0 (list (build-gate "RY" (list  pi/2) q0 )))
        (C1 (list (build-gate "RY" (list -pi/2) q1)))
        (x x0)
        (y y0)
        (z (abs z0)))

    (if (> x pi/8)
        (setf y (- y pi/8)
              z (- z pi/8)
              B0 (nconc B0 (list (build-gate "RZ" (list pi/2) q0))) ; order is order of application. i.e. apply B0 first
              B1 (nconc B1 (list (build-gate "RZ" (list -pi/2) q1)))
              C0 (cons (build-gate "RZ" (list -pi/2) q0) C0)
              C1 (cons (build-gate "RZ" (list pi/2) q1) C1))
        (setf x (+ x pi/8)
              z (- z pi/8)))

    (when (< (abs y) (abs z))
      (let ((tmp y))
        (setf y (- z)
              z (- tmp)
              A0 (list (build-gate "RX" (list pi/2) q0))
              A1 (list (build-gate "RX" (list -pi/2) q1))
              B0 (nconc B0 (list (build-gate "RX" (list -pi/2) q0)))
              B1 (nconc B1 (list (build-gate "RX" (list pi/2) q1))))))

    (when (minusp z0)
      (setf z (* -1d0 z)
            A0 (append (list (build-gate "Z" () q0)) A0 (list (build-gate "Z" () q0)))
            B0 (append (list (build-gate "Z" () q0)) B0 (list (build-gate "Z" () q0)))
            C0 (append (list (build-gate "Z" () q1)) C0 (list (build-gate "Z" () q1 )))))

    (list x y z A0 A1 B0 B1 C0 C1)))


;; Test that, when x y z is known to be in the 3 span case, that the
;; output coordinates are in the 2 span case.  When x y and z are NOT
;; in the 3 span case, this does not always hold (it seems to fail
;; about 3% of the time):

;; (assert
;;  (loop
;;    repeat 10000
;;    for (x0 y0 z0) = (make-non-2-sqisw-coord)
;;    for (x y z . _) = (canonicalize-for-sqisw x0 y0 z0 0 1)
;;    always (in-2-sqisw-span (list x y z))))


;; test that L(x0,y0,z0) =  [A0 ⊗ A1]·L(x,y,z)·[B0 ⊗ B1]·SQiSW·[C0 ⊗ C1]
;; !!!!!!!!!!!!! THIS DOESN'T PASS YET !!!!!!!!!
;; (loop
;;   for (x0 y0 z0) = (make-non-2-sqisw-coord ) 
;;   for (x y z a0 a1 b0 b1 c0 c1) = (canonicalize-for-sqisw x0 y0 z0 0 1) 
;;   repeat 1000 
;;   do (matrix-equals-dwim
;;       (canonical-representative-huang-et-al x0 y0 z0)
;;       (make-matrix-from-quil
;;        (append c1 c0
;;                (list (build-gate "SQISW" () 1 0))
;;                b1 b0
;;                (list (anon-gate "L" (canonical-representative-huang-et-al x y z) 1 0))
;;                a1 a0))))

;; Some support functions and tests for the above test:

;; (defun make-non-2-sqisw-coord ()
;;   (let* ((x (random (/ pi 4.0d0)))
;;          (y (random x))
;;          (x-y (- x y))
;;          (r (if (<= y x-y) (return-from make-non-2-sqisw-coord (make-non-2-sqisw-coord))
;;                 (+ x-y (random (- y x-y)))))
;;          (z (* r (if (zerop (random 2)) 1 -1))))
;;     (list x y z)))

;; (defun make-2-sqisw-coord ()
;;   (let* ((x (random (/ pi 4.0d0)))
;;          (y (random x))
;;          (z0 (random (min (- x y) y)))
;;          (z (* z0 (if (zerop (random 2)) 1 -1))))
;;     (the (cons double-float)  (list x y z))))

;; (defun in-weyl-chamber (coord)
;;   (destructuring-bind (x y z) coord
;;     (>= (/ pi 4.0d0) x y (abs z))))

;; (defun in-2-sqisw-span (coord)
;;   (destructuring-bind (x y z) coord
;;     (double>=  (- x y) (abs z))))

;; (assert
;;  (loop repeat 1000 for coord = (make-non-2-sqisw-coord)
;;        always (in-weyl-chamber coord)
;;        never (in-2-sqisw-span coord)))

;; (assert
;;  (loop repeat 1000 for coord = (make-2-sqisw-coord)
;;        always (and (in-2-sqisw-span coord)
;;                    (in-weyl-chamber coord))))



(define-compiler canonical-to-3-sqisw
    ((instr ("CAN" (x y z) q1 q0)
            :where (> (abs z) (- x  y))))
  (destructuring-bind
      (cx cy cz f0 f1 g0 g1 h0 h1) (canonicalize-for-sqisw (/ x 2.0d0) (/ y 2.0d0) (/ z 2.0d0) q0 q1)
    ;; L(x/2,y/2,z/2) = (F0 ⊗ F1)·L(cx,cy,cz)·(G0 ⊗ G1)·SQISW·(H0 ⊗ H1)
    ;; so we need to get the decomposition of these L(-) terms.
    (destructuring-bind
        (D0 D1 _can C0 C1) (canonical-decomposition (canonical-gate-haung-et-al
                                                     (/ x 2.0d0) (/ y 2.0d0) (/ z 2.0d0)
                                                     q1 q0))
      (declare (ignore _can)) ;; same CAN as above
      ;; So that
      ;; CAN(x,y,z) = (C0 ⊗ C1)⁺L(x/2,y/2,z/2)·(D0 ⊗ D1)⁺
      ;; hence
      ;; M = (C0 ⊗ C1)⁺·(F0 ⊗ F1)·L(cx,cy,cz)·(G0 ⊗ G1)·SQISW·(H0 ⊗ H1)·(D0 ⊗ D1)⁺
      ;;   = (C0⁺F0 ⊗ C1⁺F1)·L(cx,cy,cz)·(G0 ⊗ G1)·SQISW·(H0D0⁺ ⊗ H1D1⁺)
      (inst (dagger-inst d0))
      (mapc #'inst h0)
      (inst (dagger-inst d1))
      (mapc #'inst h1)
      (inst "SQISW" () q1 q0)
      (mapc #'inst g0)
      (mapc #'inst g1)
      
      (mapc #'inst (canonical-decomposition
                    (canonical-gate-haung-et-al
                     cx cy cz q1 q0)))
      
      (mapc #'inst f0)
      (inst (dagger-inst c0))
      (mapc #'inst f1)
      (inst (dagger-inst c1)))))


;; !!!!!!!!! DOESN'T PASS YET !!!!!!!
;; (loop
;;   repeat 1000
;;   for can = (third (canonical-decomposition (anon-gate "ANON" (random-unitary 4) 1 0)))
;;   for (x y z) = (canonical-coords can)
;;   do (assert (and (> (abs z) (- x y))
;;             (matrix-equals-dwim
;;              (gate-matrix can)
;;              (make-matrix-from-quil (canonical-to-3-sqisw can))))))


(defparameter pauli-x
  (magicl:from-list
   '(0 1
     1 0)
   '(2 2)
   :type '(complex double-float)))

(defparameter pauli-y
  (magicl:from-list
   (list 0 (- (sqrt -1))
         (sqrt -1) 0)
   '(2 2)
   :type '(complex double-float)))

(defparameter pauli-z
  (magicl:from-list
   (list 1 0 0 -1)
   '(2 2)
   :type '(complex double-float)))

;; NOTE: this function adds a dependency on magicl/ext-expokit
(defun canonical-representative-huang-et-al (x y z)
  "Calculates exp(i[p,q,r]·Σ)  where Σ = [X⊗X,Y⊗Y,Z⊗Z]"
  (declare (type double-float x y z))
  (let ((sigma (mapcar #'magicl:kron
                       (list pauli-x pauli-y pauli-z)
                       (list pauli-x pauli-y pauli-z))))
    (magicl:expm
     (magicl:.* (sqrt -1)
                (reduce #'magicl:.+
                        (mapcar #'magicl:.*
                                (list x y z)
                                sigma))))))

(defun canonical-gate-haung-et-al (x y z q1 q0)
  (declare (type double-float x y z))
  (anon-gate (format nil "L(~a,~a,~a)" x y z)
             (canonical-representative-huang-et-al x y z)
             q1 q0))
