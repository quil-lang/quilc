;;;; optimal-2q.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains attic routines of how we used to do optimal compilation
;;;; of two-qubit programs, targeting various gate sets.  This logic has been
;;;; completely subsumed by src/compilers/approx.lisp , and the non-historian
;;;; user is advised to look there instead for how we do this in the modern age.

(in-package #:cl-quil)

;;;; REMEMBER: THIS FILE IS NO LONGER MAINTAINED!
;;;;           PERUSE AT YOUR OWN RISK.
;;;;           PROBABLY DON'T MODIFY.

;;;; WARNING: THIS CODE IS TO ENSURE THIS FILE CAN CONTINUE TO BE COMPILED.

(defun euler-compiler (&rest args)
  "This is a bogus function."
  (declare (ignore args))
  (error "This is a bogus function."))


;;; Now the main file...

(define-global-counter **optimal-2q-twist-counter** generate-optimal-2q-twist-tag)

(defconstant +makhlin-distance-to-operator-distance-postfactor+ 4
  "In the invocations of Nelder-Mead in the routine OPTIMAL-2Q-COMPILE below, GOODNESS is a measure of the L^2-distance between the vectors of Makhlin invariants of the input 2Q operator and the output of the Nelder-Mead finder for the circuit template.  This is a measure of distance between the two double-cosets, which is not an exact reflection of the L^2-distance between the two operators.  In fact, it's not possible to measure that distance until the rest of the routine has completed, which picks out a particular representative element of the coset carved out by the chosen template.  Nonetheless, we have to make a decision earlier as to whether a given template is appropriate.
NOTE: I believe that even though both objects (the double-coset space and the space of projective unitaries) are compact, it actually does not suffice to scale up the distance by a constant factor (for some small fraction of gates): the Makhlin map has a degenerate derivative at the edge of the figure, so that 1/det D goes to infinity.  Bump this value up if the tests ever start failing.  A more long-lasting solution might be to work with alcove coordinates instead of Makhlin coordinates, which do not suffer from this degeneracy.")


(defun twist-to-real (m)
  "For a matrix M in SU(4), returns a values pair (SIGMA, M') such that M' = M * RZ(sigma) 1 * iSWAP and M' has real chi-gamma polynomial."
  ;; this magical formula was furnished to us by asking Mathematica to compute
  ;; the trace of M' for a symbolic M and SIGMA, then solving
  ;;     0 = imagpart(tr) = imagpart(a cos(sigma) + b sin(sigma)) ,
  ;; where a and b work out to be these disgusting sums below.
  (let* ((sigma (atan (imagpart (+ (*  1d0 (magicl:tref m 1 3) (magicl:tref m 2 0))
                                   (*  1d0 (magicl:tref m 1 2) (magicl:tref m 2 1))
                                   (*  1d0 (magicl:tref m 1 1) (magicl:tref m 2 2))
                                   (*  1d0 (magicl:tref m 1 0) (magicl:tref m 2 3))
                                   (* -1d0 (magicl:tref m 0 3) (magicl:tref m 3 0))
                                   (* -1d0 (magicl:tref m 0 2) (magicl:tref m 3 1))
                                   (* -1d0 (magicl:tref m 0 1) (magicl:tref m 3 2))
                                   (* -1d0 (magicl:tref m 0 0) (magicl:tref m 3 3))))
                      (imagpart (+ (*  1d0 (magicl:tref m 1 2) (magicl:tref m 2 0))
                                   (* -1d0 (magicl:tref m 1 3) (magicl:tref m 2 1))
                                   (*  1d0 (magicl:tref m 1 0) (magicl:tref m 2 2))
                                   (* -1d0 (magicl:tref m 1 1) (magicl:tref m 2 3))
                                   (* -1d0 (magicl:tref m 0 2) (magicl:tref m 3 0))
                                   (*  1d0 (magicl:tref m 0 3) (magicl:tref m 3 1))
                                   (* -1d0 (magicl:tref m 0 0) (magicl:tref m 3 2))
                                   (*  1d0 (magicl:tref m 0 1) (magicl:tref m 3 3)))))))
    (values
     sigma
     (reduce #'magicl:@
             (list
              m
              (su2-on-line 0 (gate-matrix (gate-definition-to-gate (lookup-standard-gate "RY")) sigma))
              (gate-matrix (gate-definition-to-gate (lookup-standard-gate "ISWAP"))))))))

(defun chi-from-evals (evals)
  "Computes the characteristic polynomial of a 4x4 matrix with all
unit-norm eigenvalues from the length 4 list of their ANGLES.  Returns
a triple (A B C) such that the characteristic polynomial is given by 1
+ (A + B i) t + C t^2 + (A - B i)t^3 + t^4."
  (declare (optimize (debug 0) (speed 3) (safety 0))
           (type list evals))
  (let ((first (first evals))
        (second (second evals))
        (third (third evals))
        (fourth (fourth evals)))
    (declare (type (complex double-float) first second third fourth))
    (let ((sum (+ first second third fourth))
          (double-sum (+ (* first second) (* first third) (* first fourth)
                         (* second third) (* second fourth)
                         (* third fourth))))
      (declare (type (complex double-float) sum double-sum))
      (list (realpart sum)
            (imagpart sum)
            (realpart double-sum)))))

(defun angles-from-su4 (m)
  "For an matrix M in SU(4) with decomposition M = O_1 D O_2 into orthogonal matrices O_1, O_2 and a diagonal matrix D, turns the VALUES pair (ANGLES, EVALS), where ANGLES is sorted descending in (-pi, pi], EVALS_j = exp(i * ANGLES_j), and D is diagonal with EVALS as diagonal entries."
  (when (minusp (realpart (magicl:det m)))
    (setf m (magicl:scale (sqrt #C(0 1)) m)))
  (let* ((ggt (reduce 'magicl:@
                      (list m
                            +e-basis+
                            (magicl:transpose +e-basis+)
                            (magicl:transpose m)
                            +e-basis+
                            (magicl:transpose +e-basis+))))
         (pairs
           (sort (mapcar (lambda (a) (cons (phase a) a))
                         (magicl:eig ggt))
                 #'> :key #'first)))
    (loop :for a :in pairs
          :collect (car a) :into angles
          :collect (cdr a) :into evals
          :finally (return (values angles evals)))))

(defun compare-circuit-angles (circuit chi &optional instr)
  "Computes a nonnegative cost function value between CHI, the result of CHI-FROM-EVALS, and the result of ANGLES-FROM-SU4 on the matrix form of CIRCUIT.  This cost function has a unique zero, corresponding to the case of equality.
The optional argument INSTR is used to canonicalize the qubit indices of the instructions in CIRCUIT."
  (when instr
    (dolist (isn circuit)
      (setf (application-arguments isn)
            (mapcar (lambda (q)
                      (qubit (position (qubit-index q) (application-arguments instr) :key #'qubit-index)))
                    (application-arguments isn)))))
  (multiple-value-bind (circuit-angles circuit-evals)
      (angles-from-su4 (make-matrix-from-quil circuit))
    (declare (ignore circuit-angles))
    (norm
     (vector-difference chi
                        (chi-from-evals circuit-evals)))))

(defun optimal-2q-compiler-for (target)
  "Return an optimal 2q compilation function for TARGET."
  (check-type target optimal-2q-target)
  (lambda (instr)
    (optimal-2q-compiler instr :target target)))

(defun optimal-2q-compiler (instr &key (target ':cz))
  "Computes a representation of a 2Q gate which is of optimal multiqubit gate depth. TARGET is of type OPTIMAL-2Q-TARGET."
  (check-type instr gate-application)
  (check-type target optimal-2q-target)

  ;; Do we have a 2q gate?
  (unless (= 2 (length (application-arguments instr)))
    (give-up-compilation))

  ;; Does it actually need compilation? If compilation won't get us
  ;; any further, we might as well just give up.
  ;;
  ;; NOTE: We used to just return (LIST INSTR) here, but Eric says
  ;; that compilers should in general get us closer to our target, and
  ;; identity is *not* getting us closer.
  (when (gate-application-trivially-satisfies-2q-target-requirements
         instr (a:ensure-list target))
    (error 'compiler-acts-trivially))

  ;; first, some utility definitions for 2Q templates that require numerical solvers
  (let ((m (gate-matrix instr)))
    (unless m
      (give-up-compilation))
    (setf m (magicl:scale (expt (magicl:det m) (/ -1 4)) m))
    (multiple-value-bind (angles evals) (angles-from-su4 m)
      ;; the basic idea is that the characteristic polynomial of m forms a
      ;; complete invariant of m inside of the double-coset space
      ;;     (SU(2) x SU(2)) * SU(4) * (SU(2) x SU(2)),
      ;; and so if we can make a hand-crafted and easy-to-compile matrix
      ;; with the same characteristic polynomial, we can backsolve to find
      ;; some easy Euler-decomposed rotations that carry the easy-to-compile
      ;; matrix to ours. this isn't *quite* accurate: the characteristic
      ;; polynomial is actually taken of "gamma(m)", where
      ;;    gamma(m) = m e e^T m^T e e^T,
      ;; but the cosets this detects are still of m.
      ;;
      ;; the core idea is taken from /0308033, and they cite /0002045.
      ;; the raw mathematics belongs to "Cartan's KAK decomposition".
      ;;
      ;; NOTE: angles-from-su4 is actually an assignment SU(4) --> C^4. it
      ;; factors through SU(4) / {I, -I}, but it **does not** factor through
      ;; SU(4) / {I, iI, -I, -iI}. accordingly, we may have to twitch m by
      ;; a factor of i to get some of the stuff below to work out.
      (let* ( ;; based on these decisions, we can start preparing the bare circuit.
             ;; the ordering in the cond expresses preference order among the
             ;; available circuit templates.
             ;;
             ;; the general case analysis was started by /0308045, and the 3-CNOT
             ;; case is cribbed from /0308033. the rest is ~unique to rigetti~
             (tag (generate-optimal-2q-twist-tag))
             (q1 (qubit-index (first (application-arguments instr))))
             (q0 (qubit-index (second (application-arguments instr))))
             (bare-circuit
               (block bare-circuit-defn
                 ;; there are a bunch of templates that we know how to test for,
                 ;; so we start by performing those tests in sequence.
                 (cond
                   ;; best case: no entanglers required
                   ((or
                     (and (double=  0 (nth 0 angles))
                          (double=  0 (nth 1 angles))
                          (double=  0 (nth 2 angles))
                          (double=  0 (nth 3 angles)))
                     (and (double= pi (nth 0 angles))
                          (double= pi (nth 1 angles))
                          (double= pi (nth 2 angles))
                          (double= pi (nth 3 angles))))
                    (return-from bare-circuit-defn nil))
                   ;; ===== depth 1 cases =====
                   ;; CNOT case
                   ((and (optimal-2q-target-meets-requirements target ':cz)
                         (double= (* pi -1/2) (nth 3 angles))
                         (double= (* pi -1/2) (nth 2 angles))
                         (double= (* pi  1/2) (nth 1 angles))
                         (double= (* pi  1/2) (nth 0 angles)))
                    (return-from bare-circuit-defn
                      (list (build-gate "RY" '(#.(/ pi 2))     q1)
                            (build-gate "CZ" '()               q0 q1)
                            (build-gate "RY" '(#.(/ pi -2))    q1))))
                   ;; ISWAP case
                   ((and (optimal-2q-target-meets-requirements target ':iswap)
                         (or
                          (and (double=      0 (nth 3 angles))
                               (double=      0 (nth 2 angles))
                               (double=     pi (nth 1 angles))
                               (double=     pi (nth 0 angles)))
                          (and (double= (- pi) (nth 3 angles))
                               (double=      0 (nth 2 angles))
                               (double=      0 (nth 1 angles))
                               (double=     pi (nth 0 angles)))
                          (and (double= (- pi) (nth 3 angles))
                               (double= (- pi) (nth 2 angles))
                               (double=      0 (nth 1 angles))
                               (double=      0 (nth 0 angles)))))
                    (return-from bare-circuit-defn
                      (list (build-gate "ISWAP" '() q1 q0))))
                   ;; CPHASE case
                   ((and (optimal-2q-target-meets-requirements target ':cphase)
                         (double= (first angles) (second angles))
                         (double= (fourth angles) (third angles))
                         (double= (second angles) (- (third angles))))
                    (let ((theta (* 2 (third angles))))
                      (return-from bare-circuit-defn
                        (list (build-gate "CPHASE" (list theta) q1 q0)))))
                   ;; PISWAP case
                   ((and (optimal-2q-target-meets-requirements target ':piswap)
                         (double= (second angles) 0d0)
                         (double= (third angles) 0d0)
                         (double= (first angles) (- (fourth angles))))
                    (let ((theta (fourth angles)))
                      (return-from bare-circuit-defn
                        (list (build-gate "PISWAP" (list theta) q1 q0)))))
                   ;; ===== depth 2 cases =====
                   ;; CNOT-CNOT case
                   ((and (optimal-2q-target-meets-requirements target ':cz)
                         (double= (- (nth 0 angles)) (nth 3 angles))
                         (double= (- (nth 1 angles)) (nth 2 angles)))
                    (let ((delta (* 1/2 (+ (nth 2 angles) (nth 3 angles))))
                          (phi   (* 1/2 (- (nth 2 angles) (nth 3 angles)))))
                      (return-from bare-circuit-defn
                        (list (build-gate "RY" '(#.(/ pi 2))    q0)
                              (build-gate "CZ" '()              q0 q1)
                              (build-gate "RY" '(#.(/ pi -2))   q0)
                              (build-gate "RX" (list (- phi))   q1)
                              (build-gate "RZ" (list (- delta)) q0)
                              (build-gate "RY" '(#.(/ pi 2))    q0)
                              (build-gate "CZ" '()              q0 q1)
                              (build-gate "RY" '(#.(/ pi -2))   q0)))))
                   ;; iSWAP-iSWAP case
                   ((and (optimal-2q-target-meets-requirements target ':iswap)
                         (double= (- (nth 0 angles)) (nth 3 angles))
                         (double= (- (nth 1 angles)) (nth 2 angles)))
                    (let ((alpha  (/ (+ (nth 2 angles) (nth 3 angles)) 2))
                          (beta   (/ (- (nth 2 angles) (nth 3 angles)) 2)))
                      (return-from bare-circuit-defn
                        (list (build-gate "ISWAP" '()          q1 q0)
                              (build-gate "RY"    (list alpha) q1)
                              (build-gate "RY"    (list beta)  q0)
                              (build-gate "ISWAP" '()          q1 q0)))))
                   ;; CNOT-iSWAP case
                   ((and (optimal-2q-target-meets-requirements target (list ':iswap ':cz))
                         (let ((shifted-angles (sort (mapcar (lambda (x) (- (mod (+ x (/ pi 2)) (* 2 pi)) pi)) angles) #'<)))
                           (and
                            (double= pi (mod (+ (first shifted-angles) (fourth shifted-angles) pi) (* 2 pi)))
                            (double= pi (mod (+ (second shifted-angles) (third shifted-angles) pi) (* 2 pi))))))
                    (let ((shifted-angles (sort (mapcar (lambda (x) (- (mod (+ x (/ pi 2)) (* 2 pi)) pi)) angles) #'<)))
                      (let* ((alpha          (*  1/2 (- (fourth shifted-angles) (third shifted-angles)   )))
                             (beta  (+ alpha (* -1/2 (- (fourth shifted-angles) (first shifted-angles) pi)))))
                        (return-from bare-circuit-defn
                          (list (build-gate "RY"    '(#.(/ pi 2))  q1)
                                (build-gate "CZ"    '()            q0 q1)
                                (build-gate "RY"    '(#.(/ pi -2)) q1)
                                (build-gate "Z"     '()            q0)
                                (build-gate "RY"    (list alpha)   q0)
                                (build-gate "RY"    (list beta)    q1)
                                (build-gate "ISWAP" '()            q0 q1)))))))

                 ;; at this point, we don't know how to test for the remaining
                 ;; depth 2 templates. we apply the relevant numerical solver, see
                 ;; if it comes up with an answer, and use it if we can.

                 ;; PiSWAP-PiSWAP case
                 (when (and (optimal-2q-target-meets-requirements target ':piswap))
                   (flet ((circuit-template (array)
                            (list
                             (build-gate "ISWAP" '()                    q0 q1)
                             (build-gate "RY"     (list (aref array 0)) q0)
                             (build-gate "RY"     (list (aref array 1)) q1)
                             (build-gate "PISWAP" (list (aref array 2)) q0 q1))))
                     (let ((x-list (chi-from-evals evals)))
                       (multiple-value-bind (template-values goodness)
                           (cl-grnm:nm-optimize (lambda (in) (compare-circuit-angles (circuit-template in) x-list instr))
                                                (make-array 3 :initial-contents '(1d0 1d0 1d0)))
                         (when (double= 0d0 (* goodness +makhlin-distance-to-operator-distance-postfactor+))
                           (return-from bare-circuit-defn (circuit-template template-values)))))))
                 ;; PiSWAP-CNOT case
                 (when (optimal-2q-target-meets-requirements target (list ':cz ':piswap))
                   (flet ((circuit-template (array)
                            (list
                             (build-gate "RY"     '(#.(/ pi 2))         q1)
                             (build-gate "CZ"     '()                   q0 q1)
                             (build-gate "RY"     '(#.(/ pi -2))        q1)
                             (build-gate "RZ"     '(#.pi)               q0)
                             (build-gate "RY"     (list (aref array 0)) q0)
                             (build-gate "RY"     (list (aref array 1)) q1)
                             (build-gate "PISWAP" (list (aref array 2)) q0 q1))))
                     (let ((x-list (chi-from-evals evals)))
                       (multiple-value-bind (template-values goodness)
                           (cl-grnm:nm-optimize (lambda (in) (compare-circuit-angles (circuit-template in) x-list instr))
                                                (make-array 3 :initial-contents (list 1d0 1d0 1d0)))
                         (when (double= 0d0 (* goodness +makhlin-distance-to-operator-distance-postfactor+))
                           (return-from bare-circuit-defn
                             (circuit-template template-values)))))))
                 ;; iSWAP-CPHASE case
                 (when (optimal-2q-target-meets-requirements target (list ':iswap ':cphase))
                   (flet ((circuit-template (array)
                            (list
                             (build-gate "ISWAP"  '()                   q0 q1)
                             (build-gate "RY"     (list (aref array 0)) q0)
                             (build-gate "RY"     (list (aref array 1)) q1)
                             (build-gate "CPHASE" (list (aref array 2)) q0 q1))))
                     (let ((x-list (chi-from-evals evals)))
                       (multiple-value-bind (template-values goodness)
                           (cl-grnm:nm-optimize (lambda (in) (compare-circuit-angles (circuit-template in) x-list instr))
                                                (make-array 3 :initial-contents (list (random 1d0) (random 1d0) (random 1d0))))
                         (when (double= 0d0 (* goodness +makhlin-distance-to-operator-distance-postfactor+))
                           (return-from bare-circuit-defn
                             (circuit-template template-values)))))))
                 ;; PiSWAP-CPHASE case
                 (when (optimal-2q-target-meets-requirements target (list ':cphase ':piswap))
                   (flet ((circuit-template (array)
                            (list
                             (build-gate "CPHASE" (list (aref array 0)) q0 q1)
                             (build-gate "RY"     (list (aref array 1)) q0)
                             (build-gate "RY"     (list (aref array 2)) q1)
                             (build-gate "PISWAP" (list (aref array 3)) q0 q1))))
                     (let ((x-list (chi-from-evals evals)))
                       (multiple-value-bind (template-values goodness)
                           (cl-grnm:nm-optimize (lambda (in) (compare-circuit-angles (circuit-template in) x-list instr))
                                                (make-array 4 :initial-contents (list (random 1d0) (random 1d0) (random 1d0) (random 1d0))))
                         (when (double= 0d0 (* goodness +makhlin-distance-to-operator-distance-postfactor+))
                           (return-from bare-circuit-defn
                             (circuit-template template-values)))))))

                 ;; ===== NON-CASES: CPHASE-CPHASE =====

                 ;; ===== depth 3 cases =====
                 ;; if we've made it down to the depth 3 case, then we can
                 ;; always succeed in making a circuit, and what's left is to
                 ;; make a decision about target architecture.
                 (cond
                   ;; CNOT-CNOT-CNOT case
                   ((optimal-2q-target-meets-requirements target ':cz)
                    (let ((alpha (+ pi (- (nth 0 angles)) (- (nth 1 angles))))
                          (beta  (+ pi (- (nth 1 angles)) (- (nth 2 angles))))
                          (delta (+ pi (- (nth 2 angles)) (- (nth 0 angles)))))
                      (list (build-gate "RY" '(#.(/ pi 2))         q0)
                            (build-gate "CZ" '()                   q0 q1)
                            (build-gate "RY" '(#.(/ pi -2))        q0)
                            (build-gate "RY" (list (* 1/2 beta))   q1)
                            (build-gate "RZ" (list (* -1/2 delta)) q0)
                            (build-gate "RY" '(#.(/ pi 2))         q1)
                            (build-gate "CZ" '()                   q0 q1)
                            (build-gate "RY" '(#.(/ pi -2))        q1)
                            (build-gate "RY" (list (* 1/2 alpha))  q1)
                            (build-gate "RY" '(#.(/ pi 2))         q0)
                            (build-gate "CZ" '()                   q0 q1)
                            (build-gate "RY" '(#.(/ pi -2))        q0))))
                   ;; iSWAP-iSWAP-iSWAP case
                   ((optimal-2q-target-meets-requirements target ':iswap)
                    ;; this case is unusual: we craft a clever prefix circuit that
                    ;; drops the remainder (as calculated by TWIST-TO-REAL) into the
                    ;; iSWAP-iSWAP case.
                    (multiple-value-bind (sigma mprime)
                        (twist-to-real m)
                      (return-from optimal-2q-compiler
                        (list* (build-gate "RY"    (list (- sigma)) q0)
                               (build-gate "ISWAP" '()              q1 q0)
                               (build-gate "Z"     '()              q0)
                               (build-gate "Z"     '()              q1)
                               (optimal-2q-compiler
                                (make-instance 'gate-application
                                               :operator (named-operator (format nil "2Q-TWIST-~D" tag))
                                               :arguments (application-arguments instr)
                                               :gate mprime)
                                :target target)))))
                   ;; failure case: no entanglers available
                   (t (return-from optimal-2q-compiler nil)))))
             ;; we also want to know a matrix representation of the bare circuit, so
             ;; that we can use MAGICL to adorn it with SU(2) x SU(2) conjugators.
             (bare-matrix
               (let ((temp (make-matrix-from-quil bare-circuit
                                                  :relabeling (standard-qubit-relabeler
                                                               (mapcar #'qubit-index
                                                                       (application-arguments instr))))))
                 (unless (= 4 (magicl:nrows temp))
                   (setf temp (kron-matrix-up temp 2)))
                 (let ((det-factor (expt (magicl:det temp) -1/4)))
                   (unless (double= 1d0 det-factor)
                     (when (minusp (imagpart det-factor))
                       (setf det-factor (* #C(0 1) det-factor)))
                     (setf temp (magicl:scale det-factor temp))))
                 temp))
             ;; u u^T and v v^T (where u is m conjugated by e, v is b-m conjugated
             ;; by e) have the same characteristic polynomial and they're both
             ;; diagonalizable, which means we can conjugate one into the other with
             ;; different choices of diagonalizing matrices: a and b.
             (a (diagonalizer-in-e-basis m))
             (b (diagonalizer-in-e-basis bare-matrix))
             ;; these have the property a^t u u^t a = d = b^t v v^t b, where d is
             ;; a diagonal matrix tracking their identical eigenvalues. eliminating
             ;; the middle equality and carrying the terms to the left, we get
             ;;     (v^dag b a^t u) (v^dag b a^t u)^t = I ,
             ;; so that c = v^dag b a^t u is in SO(4) <= SU(4)
             (c (reduce 'magicl:@
                        (list           ; v^dag = (edag . bm . e)^dag
                                        ;       = edag . bm^dag . e
                         +edag-basis+ (magicl:conjugate-transpose bare-matrix) +e-basis+
                         b
                         (magicl:transpose a)
                         ;; u = edag m e
                         +edag-basis+ m +e-basis+))))
        ;; this leaves us with the equation
        ;;     m = (e.a.b^t.edag) . bare-matrix . (e.c.edag),
        ;; where the first and final terms are necessarily in SU(2) x SU(2) <= SU(4).
        ;; we extract these "block" matrices now.
        (multiple-value-bind
              (atb1 atb0)
            (convert-su4-to-su2x2
             (reduce 'magicl:@
                     (list +e-basis+
                           a
                           (magicl:transpose b)
                           +edag-basis+)))
          (multiple-value-bind
                (c1 c0)
              (convert-su4-to-su2x2
               (reduce 'magicl:@
                       (list +e-basis+
                             c
                             +edag-basis+)))
            ;; now we just do the assembly.
            ;; convert the SU(2) matrices to instructions...
            (let ((atb1instrs (euler-compiler (make-instance 'gate-application
                                                             :gate atb1
                                                             :arguments (list (qubit q1))
                                                             :operator (named-operator
                                                                        (format nil "ATB1-~D" tag)))))
                  (atb0instrs (euler-compiler (make-instance 'gate-application
                                                             :gate atb0
                                                             :arguments (list (qubit q0))
                                                             :operator (named-operator
                                                                        (format nil "ATB0-~D" tag)))))
                  (c1instrs (euler-compiler (make-instance 'gate-application
                                                           :gate c1
                                                           :arguments (list (qubit q1))
                                                           :operator (named-operator
                                                                      (format nil "C1-~D" tag)))))
                  (c0instrs (euler-compiler (make-instance 'gate-application
                                                           :gate c0
                                                           :arguments (list (qubit q0))
                                                           :operator (named-operator
                                                                      (format nil "C0-~D" tag))))))
              ;; ... and concatenate.
              (concatenate 'list
                           c1instrs
                           c0instrs
                           bare-circuit
                           atb1instrs
                           atb0instrs))))))))

(defun optimal-2q-compile-for-defgate (gate-defn)
  "Perform CNOT-targeted optimal 2Q compilation on a DEFGATE-style matrix."
  (check-type gate-defn list)
  (optimal-2q-compiler (make-instance 'gate-application
                                      :arguments (list (qubit 1) (qubit 0))
                                      :operator (named-operator "TEMP-GATE")
                                      :gate (convert-defgate-to-magicl gate-defn))))

(defun optimal-2q-compile-to-iswap (matrix)
  (optimal-2q-compiler matrix :target ':iswap))

(defun optimal-2q-compile-to-iswap-for-defgate (gate-defn)
  "Perform CNOT-targeted optimal 2Q compilation on a DEFGATE-style matrix."
  (check-type gate-defn list)
  (optimal-2q-compile-to-iswap (convert-defgate-to-magicl gate-defn)))
