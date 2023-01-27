;;;; sqisw-decomposition.lisp
;;;;
;;;; Author: Colin O'Keefe Following the algorithm outlined in Huang
;;;; et al, https://arxiv.org/pdf/2105.06074.pdf, Supplemental
;;;; Material, I.B.

(in-package #:cl-quil)

;;; Some defintions for making sense of comments and docstrings:
;;;
;;; L(p,q,r) is the canonical representative of the class with
;;; coordinates are p,q,r.  Specifically, Huang et al define this to be
;;;
;;; L(p,q,r) = exp(i[p,q,r]·Σ)  where Σ = [X⊗X,Y⊗Y,Z⊗Z]
;;;
;;; Let  CAN(a,b,c) be the standard quil CAN gate.
;;; Let Coords(M) be the canonical coordinates (a,b,c) such that M = A·CAN(a,b,c)·B
;;; Then Coords(L(a/2,b/2,c/2)) = (a,b,c). See the following numerical demonstration:

;; (assert
;;  (loop :repeat 1000
;;        :for coords = (canonical-coords (random-unitary 4) 1 0)
;;        :for repcoords = (canonical-coords
;;                          (apply 'canonical-representative-huang-et-al
;;                                 (mapcar (lambda (x) (/ x 2)) coords))
;;                          1 0)
;;        :always (every 'double= coords repcoords)))

;;; Hence L(a/2,b/2,c/2) = A·CAN(a,b,c)·B for some A,B ∈ SU(2)⊗SU(2)
;;;
;;; Let ~ denote "local equivalence".


;;; 2 SQISWAP CASE

(defun interleaving-1-qubit-gates (x y z q0 q1)
  "x y and z are canonical parameters in the Weyl chamber satisfying
pi/4 >= x >= y >= |z|. Furthermore, this function assumes that the
class of gates indicated by x,y,z falls within the span of 2 SQISWAP
gates, which is equivalent to the additional condtion that x >= y + |z|.

q0 and q1 are qubit indices.

Returns a list of two members, L0 and L1, both lists of single-qubit
gate applications, acting on q0 and q1 respectively.  L0 is a list of
three rotations RZ(γ) RX(α) RZ(γ), and L1 is a list containing one
RX(β).

L(x,y,z) ~ SQISWAP [ RZ(γ)RX(α)RZ(γ) ⊗ RX(β)] SQISWAP

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

(define-compiler canonical-to-2-sqiswap
    ((instr ("CAN" (x y z) q1 q0)
            :where (<= (abs z) (- x y)))) ; this will be true for ≅ 80% of all coordinates x y z
  (destructuring-bind
      (c0 c1) (interleaving-1-qubit-gates (/ x 2.0d0) (/ y 2.0d0) (/ z 2.0d0) q0 q1) ; L(v/2) ~ CAN(v)
    (destructuring-bind
        (e0 e1 can2 d0 d1)
        (canonical-decomposition
         (anon-gate "V" (make-matrix-from-quil
                         (append
                          (list (build-gate "SQISWAP" () q1 q0))
                          c0 c1
                          (list (build-gate "SQISWAP" () q1 q0))))
                    q1 10))
      (declare (ignore can2))
      (inst (anon-gate "E0t" (magicl:conjugate-transpose (gate-matrix e0)) q0))
      (inst (anon-gate "E1t" (magicl:conjugate-transpose (gate-matrix e1)) q1))
      (inst "SQISWAP" () q1 q0)
      (mapc #'inst c0)
      (mapc #'inst c1)
      (inst "SQISWAP" () q1 q0)
      (inst (anon-gate "D0t" (magicl:conjugate-transpose (gate-matrix d0)) q0))
      (inst (anon-gate "D1t" (magicl:conjugate-transpose (gate-matrix d1)) q1)))))


;; Demonstration that canonical-to-2-sqiswap works:

;; (assert
;;  (loop
;;    :repeat 1000
;;    :for (a b c) = (make-2-sqiswap-coord)
;;    :for can = (build-gate "CAN" (list (* 2.0d0 a) (* 2.0d0 b) (* 2.0d0 c)) 1 0)
;;    :always (matrix-equals-dwim
;;             (gate-matrix can)
;;             (make-matrix-from-quil (canonical-to-2-sqiswap can)))))


;;; 3 SQISWAP CASE

;; A general description of the three SQISWAP case: Let W be the Weyl
;; chamber and let W' be the volume of W that is covered by two SQISWAP
;; applications. Let (x,y,z) ∈ W\W'.  We can find (x',y',z') ∈ W'
;; such that L(x',y',z') = M·L where
;;
;;        M = Rᵦᵦ(±π/4)⊗Rᵧᵧ(±π/4) ~ SQISWAP
;;
;; and
;;
;;        L ~ L(x,y,z)
;;
;; And because M is unitary, this leads directly to an expression of
;; L(x,y,z) in terms of L(x',y',z') and one SQISWAP gate.
;;
;; To determine M, we need to express (x',y',z') in terms of (x,y,z)
;; explicitly. First notice the following, as an example of a more
;; general property:
;;
;;         L(x+π/8, y, z) = L(x,y,z) R_xx(-π/4)
;;
;; because the R_ii all commute, and L(-) is a product of R_ii's. The
;; paper shows that it is always possible to add π/8 to one
;; coordinate and subtract π/8 from another, and, possibly after
;; rearranging them, to get the desired (x',y',z') in W'. This fact,
;; in conjuntion with the observation above, shows that we can find such an M
;;
;; To get equality, we utilize `canonical-decomposition` to
;; find interleaving single-qubit unitaries such that
;;
;;            L(x,y,z) = A·L(x',y',z')·B·C·SQISWAP·D
;;
;; We then translate the L's into CAN gates, again using
;; canonical-decomposition.

;; first, some leg work
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

(defparameter +xx+
  (magicl:kron pauli-x pauli-x))

(defparameter +yy+
  (magicl:kron pauli-y pauli-y))

(defparameter +zz+
  (magicl:kron pauli-z pauli-z))

(defparameter +ii+
  (magicl:eye 4 :type '(complex double-float)))

(defun interaction-matrix (theta interaction)
  "THETA is an angle in radians. INTERACTION is a matrix, meant to be
one of +xx+ +zz+ or +yy+.

Calculates exp(-i(theta/2) M) = cos(theta/2)I⊗I - i·sin(theta/2)M
where M is the interaction matrix.
"
  (magicl:.+
   (magicl:.* (cos (/ theta 2.0d0)) +ii+)
   (magicl:.* (* -1.0d0 (sqrt -1) (sin (/ theta 2.0d0)))
              interaction)))

(defun rzz (theta) (interaction-matrix theta +zz+))
(defun ryy (theta) (interaction-matrix theta +yy+))
(defun rxx (theta) (interaction-matrix theta +xx+))

;; A felicitous property of these rzz rxx ryy is that they all commute
;; with one another:
;;
;; (assert
;;  (loop :repeat 1000
;;        :for a1 = (random pi/2)
;;        :for a2 = (random pi/2)
;;        :for m1 = (funcall (elt '(rxx ryy rzz) (random 3)) a1)
;;        :for m2 = (funcall (elt '(rxx ryy rzz) (random 3)) a2)
;;        :always (matrix-equals-dwim
;;                 (magicl:@ m1 m2)
;;                 (magicl:@ m2 m1))))
;;
;; A second lucky break  is that (rii theta) and (rjj theta)
;; have the same canonical coodinates:
;;
;; (loop :repeat 1000
;;       :for theta = (random pi/2)
;;       :for coords1 = (canonical-coords
;;                       (funcall (elt '(rxx ryy rzz) (random 3)) theta)
;;                       1 0)
;;       :for coords2 = (canonical-coords
;;                       (funcall (elt '(rxx ryy rzz) (random 3)) theta)
;;                       1 0)
;;       :always (every #'double= coords1 coords2))
;;
;; Hence, they're all locally equivalent.
;;
;; And a final nice property is that
;;
;;    Rᵦᵦ(±π/4)⊗Rᵧᵧ(±π/4) ~ SQISWAP
;;    when β ≠ γ and β,γ ∈ {X,Y,Z}

(defun canonical-representative-huang-et-al (x y z)
  "Calculates exp(i[x,y,z]·Σ)  where Σ = [X⊗X,Y⊗Y,Z⊗Z]"
  (declare (type double-float x y z))
  ;; exp(i[x,y,z]·[+xx+,+yy+,+zz+])
  ;; =  exp(ix * +xx+) exp(iy * +yy+) exp(iz * +zz+)
  (magicl:@
   (interaction-matrix (* -2 x) +xx+)
   (interaction-matrix (* -2 y) +yy+)
   (interaction-matrix (* -2 z) +zz+)))

(defun canonical-gate-haung-et-al (x y z q1 q0)
  (declare (type double-float x y z))
  (anon-gate (format nil "L(~a,~a,~a)" x y z)
             (canonical-representative-huang-et-al x y z)
             q1 q0))

(defun shift-into-sqiswap-depth-2 (x0 y0 z0)
  "Given x0 y0 z0 in W\W' return a list 7 values (x y z x1 y1 z1 M) such that

L(x1,y1,z1) = L(x,y,z)·M 
M ~ SQISWAP 
L(x0,y0,z0) ~ L(x1,y1,z1)
"
  (declare (type double-float x0 y0 z0))
  (let ((x x0)
        (y y0)
        (z (abs z0))
        (xfactor 0)
        (yfactor 0)
        (zfactor 0)
        (pi/4 (/ pi 4.0d0))
        (-pi/4 (- (/ pi 4.0d0)))
        (yz-flip nil)
        (yz-sign-change 1))
    (if (> x pi/8)
        (setf y (- y pi/8)
              z (- z pi/8)
              yfactor pi/4       ; see comment above for derivation of these factors
              zfactor pi/4)
        (setf x (+ x pi/8)
              z (- z pi/8)
              xfactor -pi/4
              zfactor pi/4))

    (when (< (abs y) (abs z))
      (let ((tmp y))
        (setf y (- z)
              z (- tmp)
              tmp yfactor
              yfactor (- zfactor)
              zfactor (- tmp)
              yz-flip t)))

    (when (minusp z0)
      (setf z (* -1d0 z)
            zfactor (- zfactor)
            yz-sign-change (* yz-sign-change -1)))

    (list x y z
          x0
          (if yz-flip (- (abs z0)) y0)
          (* yz-sign-change (if yz-flip (- y0) (abs z0)))
          (magicl:dagger (magicl:@ (rxx xfactor) (ryy yfactor) (rzz zfactor))))))

;; Test that M ~ SQISWAP, L(x0,y0,z0) ~ L(x1,y1,z1) and (x,y,z) in W' and L(v1) = L(v).M:

;; (assert
;;  (loop :repeat 10
;;        :for (x0 y0 z0) = (make-non-2-sqiswap-coord)
;;        :for (x y z x1 y1 z1 M) = (shift-into-sqiswap-depth-2 x0 y0 z0)
;;        :always (in-2-sqiswap-span (list x y z))
       
;;        :always (every #'double=
;;                       (canonical-coords (canonical-gate-haung-et-al x0 y0 z0 1 0))
;;                       (canonical-coords (canonical-gate-haung-et-al x1 y1 z1 1 0)))
       
;;        :always (every #'double=
;;                       (canonical-coords M 1 0)
;;                       (canonical-coords (build-gate "SQISWAP" () 1 0)))

;;        :always (matrix-equals-dwim
;;                 (canonical-representative-huang-et-al x1 y1 z1)
;;                 (magicl:@ (canonical-representative-huang-et-al x y z)
;;                           m))))

(defun same-can-p (can1 can2)
  (every #'double=
         (mapcar #'constant-value (application-parameters can1))
         (mapcar #'constant-value (application-parameters can2))))

(defun express-m-as-sqiswap (m q1 q0)
  "For M ~ SQISWAP, returns a list (SB0 SB1 SA0 SA1) such that M = [SA0⊗SA1]·SQISWAP·[SB0⊗SB1]"
  (destructuring-bind
      (b0 b1 can1 a0 a1) (canonical-decomposition (anon-gate "M" m q1 q0))
    (destructuring-bind
        (d0 d1 can2 c0 c1) (canonical-decomposition (build-gate "SQISWAP" ()  q1 q0))
      (assert (same-can-p can1 can2) (m) "Unitary is not locally eqivlent to SQISWAP.")
      ;; M = A CAN B
      ;; SQISWAP = C CAN D
      ;; => M = A.Ct.SQISWAP.Dt.B
      (list
       (anon-gate "SB0" 
                  (magicl:@ (magicl:dagger (gate-matrix D0))
                            (gate-matrix B0))
                  q0)
       (anon-gate "SB1" 
                  (magicl:@ (magicl:dagger (gate-matrix D1))
                            (gate-matrix B1))
                  q1)
       (anon-gate "SA0"
                  (magicl:@ (gate-matrix A0)
                            (magicl:dagger (gate-matrix C0)))
                  q0)
       (anon-gate "SA1"
                  (magicl:@ (gate-matrix A1)
                            (magicl:dagger (gate-matrix C1)))
                  q1)))))

;; Check that M = A.SQISWAP.B

;; (assert
;;  (loop :repeat 1000
;;        :for m = (seventh (apply 'shift-into-sqiswap-depth-2 (make-non-2-sqiswap-coord)))
;;        :for (b0 b1 a0 a1) = (express-m-as-sqiswap m 1 0)
;;        :always (matrix-equals-dwim
;;                 m
;;                 (make-matrix-from-quil
;;                  (list b0 b1 (build-gate "SQISWAP" () 1 0) a0 a1)))))

(define-compiler canonical-to-3-sqiswap
    ((instr ("CAN" (x0 y0 z0) q1 q0)
            :where (> (abs z0) (- x0  y0))))
  (destructuring-bind
      (x y z x1 y1 z1 M) (shift-into-sqiswap-depth-2 (/ x0 2.0d0) (/ y0 2.0d0) (/ z0 2.0d0))
    ;; CAN(x0,y0,z0) ~ L(x0/2,y0/2,z0/2) ~ L(x1,y1,z1)
    (destructuring-bind
        (B0 B1 can A0 A1) (canonical-decomposition (canonical-gate-haung-et-al x1 y1 z1 q1 q0))
      (assert (same-can-p instr can))
      ;; L(x1,y1,z1) = A.CAN(x0,y0,z0).B => CAN(x0,y0,z0) = At.L(x1,y1,z1).Bt
      ;; But L(x1,y1,z1) = L(x,y,z).M
      ;; So we first factor M:
      (destructuring-bind
          (SB0 SB1 SA0 SA1) (express-m-as-sqiswap M q1 q0)
        ;; M = SA.SQISWAP.SB
        ;; Giving us CAN(x0,y0,z0) = At.L(x,y,z).SA.SQISWAP.SB.Bt
        ;; Finally, we want to emit a CAN gate,so  we decompse L(x,y,z)
        (let ((L-decomp (canonical-decomposition (canonical-gate-haung-et-al x y z q1 q0))))
          ;; Leaving us with:
          (inst (anon-gate "B0t" (magicl:dagger (gate-matrix B0)) q0))
          (inst (anon-gate "B1t" (magicl:dagger (gate-matrix B1)) q1))
          (inst SB0)
          (inst SB1)
          (inst (build-gate "SQISWAP" () q1 q0))
          (inst SA0)
          (inst SA1)
          (mapc #'inst L-decomp)
          (inst (anon-gate "A0t" (magicl:dagger (gate-matrix A0)) q0))
          (inst (anon-gate "A1t" (magicl:dagger (gate-matrix A1)) q1)))))))

;; (assert
;;  (loop :repeat 1000
;;        :for can = (build-gate "CAN" (make-non-2-sqiswap-coord) 1 0)
;;        :for decomp = (canonical-to-3-sqiswap can)
;;        :always (safe-m=
;;                 (gate-matrix can)
;;                 (make-matrix-from-quil decomp))))

;;; UTILITIES FOR TESTS

;; (defun make-non-2-sqiswap-coord ()
;;   "Make a coordinate in the Weyl chamber as defined by Huang et al
;; that cannot be covered by 2 SQISWAP gates."
;;   (let* ((x (random (/ pi 4.0d0)))
;;          (y (random x))
;;          (x-y (- x y))
;;          (r (if (<= y x-y) (return-from make-non-2-sqiswap-coord (make-non-2-sqiswap-coord))
;;                 (+ x-y (random (- y x-y)))))
;;          (z (* r (if (zerop (random 2)) 1 -1))))
;;     (list x y z)))

;; (defun make-2-sqiswap-coord ()
;;   "Make a coordinate in the Weyl chamber as defined by Huang et al that
;; can be covered by 2 SQISWAP gates."
;;   (let* ((x (random (/ pi 4.0d0)))
;;          (y (random x))
;;          (z0 (random (min (- x y) y)))
;;          (z (* z0 (if (zerop (random 2)) 1 -1))))
;;     (the (cons double-float)  (list x y z))))

;; (defun in-weyl-chamber (coord)
;;   "COORD is a list of 3 numbers. Returns T if those numbers represent a
;; canonical coordinate in the Weyl chamber as defined by Huang et al. "
;;   (destructuring-bind (x y z) coord
;;     (>= (/ pi 4.0d0) x y (abs z))))

;; (defun in-2-sqiswap-span (coord)
;;   "COORD is a list of three numbers. Returns T if those numbers
;; represent a coordinate in the Weyl chamber as defined by Huang et al
;; such the canonical representative whose coordinates are COORD can be
;; covered by two SQISWAP gates. "
;;   (destructuring-bind (x y z) coord
;;     (double>=  (- x y) (abs z))))

;; (assert
;;  (loop :repeat 1000 for coord = (make-non-2-sqiswap-coord)
;;        :always (in-weyl-chamber coord)
;;        :never (in-2-sqiswap-span coord)))

;; (assert
;;  (loop :repeat 1000 for coord = (make-2-sqiswap-coord)
;;        :always (and (in-2-sqiswap-span coord)
;;                     (in-weyl-chamber coord))))

;; (defun canonical-coords (gate &rest params-and-qubits)
;;   "Pluck the parameters from the canonical gate found in the
;; decomposition of GATE. These are NOT the coordinates defined by Huang
;; et al."
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

;; (defun safe-m= (m1 m2)
;;   (handler-case
;;       (matrix-equals-dwim m1 m2)
;;     (error () nil)))
