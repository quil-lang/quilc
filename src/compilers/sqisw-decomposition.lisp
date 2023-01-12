;;;; sqisw-decomposition.lisp
;;;;
;;;; Author: Colin O'Keefe Following the algorithm outlined in
;;;; https://arxiv.org/pdf/2105.06074.pdf, Supplemental Material, I.B.

(in-package #:cl-quil)

(defun interleaving-1-qubit-gates (x y z q0 q1)
  "x y and z are canonical parameters in the Weyl chamber satisfying pi/4 >= x >= y >= |z|. Furthermore, this function assumes that the class of gates indicated by x,y,z falls within the span of 2 SQISW gates, which is equivalent to the additional condtion that x >= y + |z|. 

q0 and q1 are qubit indices

Returns two values L0 and L1, both lists of single-qubit gate applications, acting on q0 and q1 respectively. 
L0 is a list of three rotations RZ(γ) RX(α) RZ(θ), and L1 is a list containing one RX(β).

L(x,y,z) ~ SQiSW [ RZ(γ)RX(α)RZ(γ) ⊗ RX(β)] SQiSW

Where L(x,y,z) is the canonical representative of the class with coordinates x,y, and z."
  (declare (type double-float x y z))
  (assert (xyz-in-2-sqisw-span-p x y z) () "~a ~a ~a not in the span of 2 SQISW gates." x y z)
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
         (gamma (acos (* (cond ((plusp z) 1d0) ((minusp z) -1d0) (t 0d0)) ; sign(z)
                         (sqrt (/ (* 4 cos-x-sq cos-z-sq sin-y-sq)
                                  (+ (* 4 cos-x-sq cos-z-sq sin-y-sq)
                                     (* cos2x cos2y cos2z))))))))
    (values
     (list (build-gate "RZ" (list gamma) q0)
           (build-gate "RX" (list alpha) q0 )
           (build-gate "RZ" (list gamma) q0))
     (list (build-gate "RX" (list beta) q1)))))


(defun xyz-in-2-sqisw-span-p (x y z)
  (and (in-weyl-chamber-p x y z)
       (>= x (+ y (abs z)))))

(defun in-weyl-chamber-p (x y z)
  (>= #.(/ pi 4.0d0) x y (abs z)))

(defun canonicalize-for-sqisw (x0 y0 z0 q0 q1)
  "x0 y0 and z0 are canonical parameters in the Weyl chamber satisfying pi/4 >= x >= |z|. q0 and q1 are qubit indices.

  
Returns 9 values. x y z A0 A1 B0 B1 C0 C1 with the following interpretation.  
If x0,y0,z0 are canonical paramters for an arbitrary 2 qubit gate, then that gate is equivalent to 

     [A0 ⊗ A1] L(x,y,z) [C0 ⊗ C1] SQiSW [B0 ⊗ B1] 

where L(x,y,z) is the canoncal representative of the class for whose
coordinates are x,y,z and is also a gate in the span of two SQiSW
applications.
"
  (assert (in-weyl-chamber-p x0 y0 z0) () "~a, ~a, ~a are not in the Weyl Chamber" x0 y0 z0)
  (let ((A0 (list))                     ; essentially, the identity
        (A1 (list))
        (B0 (list (build-gate "RY" (list -pi/2) q0)))
        (B1 (list (build-gate "RY" (list pi/2) q1)))
        (C0 (list (build-gate "RY" (list  pi/2) q0 )))
        (C1 (list (build-gate "RY" (list -pi/2) q1)))
        (x x0)
        (y y0)
        (z (abs z0)))
    (if
     (> x pi/8)
     (setf y (- y pi/8 )
           z (- z pi/8)
           B0 (nconc B0 (list (build-gate "RZ" (list pi/2) q0))) ; order is order of application. i.e. apply B0 first
           B1 (nconc B1 (list (build-gate "RZ" (list -pi/2) q1)))
           C0 (cons (build-gate "RZ" (list -pi/2) q0) C0)
           C1 (cons (build-gate "RZ" (list pi/2) q1) C1))
     (setf x (+ x pi/8)
           z (- z pi/8)))

    (when (< (abs y) (abs z))
      (let ((tmp y))
        (setf y (* -1 z)
              z (* -1 tmp)
              A0 (list (build-gate "RX" (list pi/2) q0))
              A1 (list (build-gate "RX" (list -pi/2) q1))
              B0 (nconc B0 (list (build-gate "RX" (list -pi/2) q0)))
              B1 (nconc B1 (list (build-gate "RX" (list pi/2) q1))))))
    
    (when (minusp z0)
      (setf z (* -1 z)
            A0 (append (list (build-gate "Z" () q0)) A0 (list (build-gate "Z" () q0)))
            B0 (append (list (build-gate "Z" () q0)) B0 (list (build-gate "Z" () q0)))
            C1 (append (list (build-gate "Z" () q1)) C1 (list (build-gate "Z" () q1 )))))
    (list x y z A0 A1 B0 B1 C0 C1)))


(define-compiler sqisw-decompose
    ((instr (_ _ q1 q0)))
  "This compiler will produce a sequence containing at most two SQISW
applications whenever possible, and, when it cannot, is guaranteed to
produce a sequence containing at most three SQiSW applications."
  (destructuring-bind
      (b0 b1 can1 a0 a1) (canonical-decomposition instr)
    (destructuring-bind
        ;; making an assumption that I can divide by two here.
        ;; TODO: check that this isn't bunk.
        (x y z) (loop :for p :in (application-parameters can1)
                      :collect (/ (constant-value p) 2d0))
      (cond
        ((<= (abs z) (- x y))
         (destructuring-bind
             (c0 c1) (interleaving-1-qubit-gates x y z q0 q1)
           (let ((v (append (list (build-gate "SQISW" () q1 q0))
                            c0 c1
                            (list (build-gate "SQISW" () q1 q0)))))
             (destructuring-bind
                 (e0 e1 can2 d0 d1) (canonical-decomposition (anon-gate "V" (make-matrix-from-quil v)  q1 q0))
               (declare (ignore can2))
               (inst b0)
               (inst (anon-gate "E0t" (magicl:conjugate-transpose (gate-matrix e0)) q0))
               (inst b1)
               (inst (anon-gate "E1t" (magicl:conjugate-transpose (gate-matrix e1)) q1))
               (inst "SQISW" () q1 q0)
               (mapc #'inst c0)
               (mapc #'inst c1)
               (inst "SQISW" () q1 q0)
               (inst (anon-gate "D0t" (magicl:conjugate-transpose (gate-matrix d0)) q0))
               (inst a0)
               (inst (anon-gate "D1t" (magicl:conjugate-transpose (gate-matrix d1)) q1))
               (inst a1)))))

        (t
         (destructuring-bind
             (cx cy cz f0 f1 g0 g1 h0 h1) (canonicalize-for-sqisw x y z q0 q1)
           (destructuring-bind
               (c0 c1) (interleaving-1-qubit-gates cx cy cz q0 q1)
             (let ((v (append (list (build-gate "SQISW" () q1 q0))
                              c0 c1
                              (list (build-gate "SQISW" () q1 q0)))))
               (destructuring-bind
                   (e0 e1 can2 d0 d1) (canonical-decomposition (anon-gate "V" (make-matrix-from-quil v) q1 q0))
                 (declare (ignore can2))
                 (inst b0)
                 (mapc #'inst h0)
                 (inst b1)
                 (mapc #'inst h1)
                 (inst "SQISW" () q1 q0)
                 (mapc #'inst g0)
                 (inst (anon-gate "E0t" (magicl:conjugate-transpose (gate-matrix e0)) q0))
                 (mapc #'inst g1)
                 (inst (anon-gate "E1t" (magicl:conjugate-transpose (gate-matrix e1)) q1))
                 (inst "SQISW" () q1 q0)
                 (mapc #'inst c0)
                 (mapc #'inst c1)
                 (inst "SQISW" () q1 q0)
                 (inst (anon-gate "D0t" (magicl:conjugate-transpose (gate-matrix d0)) q0))
                 (mapc #'inst f0)
                 (inst a0)
                 (inst (anon-gate "D1t" (magicl:conjugate-transpose (gate-matrix d1)) q1))
                 (mapc #'inst f1)
                 (inst a1))))))))))














