(in-package #:cl-quil-tests)


(defun make-non-2-sqiswap-coord ()
  "Make a coordinate in the Weyl chamber as defined by Huang et al
whose gate class cannot be covered by 2 SQISWAP gates."
  (let* ((x (random (/ pi 4.0d0)))
         (y (random x))
         (x-y (- x y))
         (r (if (<= y x-y) (return-from make-non-2-sqiswap-coord (make-non-2-sqiswap-coord))
                (+ x-y (random (- y x-y)))))
         (z (* r (if (zerop (random 2)) 1 -1))))
    (list x y z)))

(defun make-2-sqiswap-coord ()
  "Make a coordinate in the Weyl chamber as defined by Huang et al whose
gate class can be covered by 2 SQISWAP gates."
  (let* ((x (random (/ pi 4.0d0)))
         (y (random x))
         (z0 (random (min (- x y) y)))
         (z (* z0 (if (zerop (random 2)) 1 -1))))
    (the (cons double-float)  (list x y z))))

(defun in-weyl-chamber (coord)
  "COORD is a list of 3 numbers. Returns T if those numbers represent a
canonical coordinate in the Weyl chamber as defined by Huang et al. "
  (destructuring-bind (x y z) coord
    (>= (/ pi 4.0d0) x y (abs z))))

(defun in-2-sqiswap-span (coord)
  "COORD is a list of three numbers. Returns T if those numbers
represent a coordinate in the Weyl chamber as defined by Huang et al
such the canonical representative whose coordinates are COORD can be
covered by two SQISWAP gates. "
  (destructuring-bind (x y z) coord
    (cl-quil::double>=  (- x y) (abs z))))

(defun canonical-coords (gate &rest params-and-qubits)
  "Pluck the parameters from the canonical gate found in the
decomposition of GATE. These are NOT the coordinates defined by Huang
et al."
  (let ((gate
          (etypecase gate
            (gate-application gate)
            (magicl:matrix/complex-double-float
             (apply #'anon-gate "ANON" gate params-and-qubits))
            (string
             (apply #'build-gate gate params-and-qubits)))))
    (mapcar #'constant-value
            (application-parameters
             (third (cl-quil::canonical-decomposition gate))))))

(defun safe-m= (m1 m2)
  "Just want it to return nil in case m1 m2 not in the same projective class."
  (handler-case
      (cl-quil::matrix-equals-dwim m1 m2)
    (error () nil)))


(deftest test-canonical-parameter-assumptions ()
  "Let L := L(p,q,r) := exp(i[p,q,r]·Σ) where Σ := [X⊗X,Y⊗Y,Z⊗Z] and let
CAN(a,b,c) be quilc's CAN gate. 

Let Coords(M) be the canonical coordinates (a,b,c) such that 

     M = A·CAN(a,b,c)·B 

for some A,B in SO(2)⊗SO(2).

This test ensures that Coords(L(a/2,b/2,c/2)) = (a,b,c). "
  (loop :repeat 1000
        :for coords = (canonical-coords (cl-quil::random-unitary 4) 1 0)
        :for repcoords = (canonical-coords
                          (apply 'cl-quil::canonical-representative-huang-et-al
                                 (mapcar (lambda (x) (/ x 2)) coords))
                          1 0)
        :always (every 'cl-quil::double= coords repcoords)))


(deftest test-canonical-to-2-sqiswap ()
  "For a CAN(x/2,y/2,z/2) gate whose parameters meet the condition
that (>= (- x y) (abs z)), this test ensures that the gate can be
decomosed into two SQISWAP gates interleaved by single qubit
rotations."
  (is
      (loop
        :repeat 1000
        :for (a b c) = (make-2-sqiswap-coord)
        :for can = (cl-quil::build-gate "CAN" (list (* 2.0d0 a) (* 2.0d0 b) (* 2.0d0 c)) 1 0)
        :always (cl-quil::matrix-equals-dwim
                 (cl-quil::gate-matrix can)
                 (cl-quil::make-matrix-from-quil (cl-quil::canonical-to-2-sqiswap can))))))



(deftest test-interaction-matrix-assumptions ()
  "Ensures that rxx, ryy, rzz freely commute with one another and Rᵦᵦ(±π/4)⊗Rᵧᵧ(±π/4) ~ SQISWAP when β ≠ γ and β,γ ∈ {X,Y,Z}"
  (let ((interactions
          '(cl-quil::rxx cl-quil::ryy cl-quil::rzz))
        (sqiswap-coords
          (canonical-coords "SQISWAP" ()  1 0))
        (pis/4
          (list (/ pi 4.0d0) (- (/ pi 4.0d0)))))
    ;; the rii commute
    (is
        (loop :repeat 1000
              :for a1 = (random pi/2)
              :for a2 = (random pi/2)
              :for m1 = (funcall (elt interactions (random 3)) a1)
              :for m2 = (funcall (elt interactions (random 3)) a2)
              :always (cl-quil::matrix-equals-dwim
                       (magicl:@ m1 m2)
                       (magicl:@ m2 m1))))
    ;; rii(θ).rjj(θ) ~ SQISWAP when |θ| = π/4 and ii ≠ jj 
    (is
        (loop
          :for rii :in interactions
          :always
          (loop
            :for rjj :in (remove rii interactions)
            :always 
            (loop
              :for p1 :in pis/4
              :always
              (loop
                :for p2 :in pis/4
                :always
                (every #'cl-quil::double=
                       sqiswap-coords
                       (canonical-coords (magicl:@ (funcall rii p1)
                                                   (funcall rjj p2))
                                         1 0)))))))))


(deftest test-shift-int-sqiswap-sapn-2 ()
  "(shift-into-sqiswap-span-2 x0 y0 z0) returns a list (x y z x1 y1 z1 M).  This test

checks that that M ~ SQISWAP, that L(x0,y0,z0) ~ L(x1,y1,z1),  that 
 (x,y,z) is in the 2 sqiswap span region of the Weyl chamber, and that
 L(v1) = L(v).M:"

  (is
      (loop :repeat 1000
            :for (x0 y0 z0) = (make-non-2-sqiswap-coord)
            :for (x y z x1 y1 z1 M) = (cl-quil::shift-into-sqiswap-span-2 x0 y0 z0)
            :always (in-2-sqiswap-span (list x y z))
            
            :always (every #'cl-quil::double=
                           (canonical-coords (cl-quil::canonical-gate-huang-et-al x0 y0 z0 1 0))
                           (canonical-coords (cl-quil::canonical-gate-huang-et-al x1 y1 z1 1 0)))
            
            :always (every #'cl-quil::double=
                           (canonical-coords M 1 0)
                           (canonical-coords "SQISWAP" () 1 0))
            
            :always (cl-quil::matrix-equals-dwim
                     (cl-quil::canonical-representative-huang-et-al x1 y1 z1)
                     (magicl:@ (cl-quil::canonical-representative-huang-et-al x y z)
                               m)))))

(deftest test-express-m-as-sqiswap ()
  "(express-m-as-sqiswap m q1 q0) returns a list (B0 B1 A0 A1) Check that M = [A0⊗A1]·SQISWAP·[B0⊗B1]"
  (is
      (loop :repeat 1000
            :for m = (seventh (apply 'cl-quil::shift-into-sqiswap-span-2 (make-non-2-sqiswap-coord)))
            :for (b0 b1 a0 a1) = (cl-quil::express-m-as-sqiswap m 1 0)
            :always (cl-quil::matrix-equals-dwim
                     m
                     (cl-quil::make-matrix-from-quil
                      (list b0 b1 (cl-quil::build-gate "SQISWAP" () 1 0) a0 a1))))))


(deftest test-canonical-to-3-sqiswap ()
  "Checks that a CAN(x,y,z) gate whose coordinates satisfy (> (abs z) (-
x y)) can be decomposed into three SQISWAP gates interleaved by single
qubit gates."
  (is
      (loop :repeat 1000
            :for can = (cl-quil::build-gate "CAN" (make-non-2-sqiswap-coord) 1 0)
            :for decomp = (cl-quil::canonical-to-3-sqiswap can)
            :always (safe-m=
                     (cl-quil::gate-matrix can)
                     (cl-quil::make-matrix-from-quil decomp)))))
