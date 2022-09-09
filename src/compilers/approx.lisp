;;;; approx.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; This file contains routines that use a family of circuit templates to emit
;;;; from among them a "best approximation" to a two-qubit program.  This sort of
;;;; protocol was previously discussed by arXiv:1811.12926 Appendix B, which is a
;;;; good reference for the overall ideas here. This implementation comes with a
;;;; number of features and caveats:
;;;;
;;;; * The definition of "best approximation" is that the average fidelity of a
;;;;   state passed through the original program vs the emitted circuit is
;;;;   maximized from among the various template options.
;;;;
;;;; * The global variable *ENABLE-APPROXIMATE-COMPILATION* controls whether the
;;;;   routine will tolerate imperfect maximal fidelities.  This can be used to
;;;;   two effects: given a limited set of templates, the routine will emit the
;;;;   best option without complaint; or, given fidelity information about the
;;;;   native operations on the chip, the routine will take this into account
;;;;   and emit the template that has the highest fidelity **after incorporating
;;;;   the fidelity loss due to the operations themselves**.
;;;;
;;;; * The model which blends of the fidelity loss of the operations with the
;;;;   fidelity loss of selecting an imperfect template is their product.  This
;;;;   is an imperfect approximation for any number of reasons, including
;;;;   potential deconstructive interference of unitary error, potentially
;;;;   resulting in a lower overall fidelity loss than expected.  (This is less
;;;;   likely with nonunitary error, though I have not worked this out precisely.)
;;;;
;;;; * The templates defined here are user-extensible and are drawn from several
;;;;   places, including arXiv:0308033 and our forthcoming compilation paper.

(in-package #:cl-quil)


;;; what "shortest gate string" means depends upon what gateset is available.
;;; the TARGET argument is either an OPTIMAL-2Q-TARGET-ATOM or a(n unsorted)
;;; sequence of such atoms, indicating which 2Q gates are available for use.

;; NOTE: editors of the following list should probably also investigate the
;;       routines immediately following this definition.
(deftype optimal-2q-target-atom ()
  "An enumerative type describing the two-qubit operators required by a particular two-qubit decomposition template."
  '(member :cz :iswap :piswap :xy :cphase :cnot))

(defun sequence-of-optimal-2q-target-atoms-p (seq)
  (and (typep seq 'sequence)
       (every (lambda (a) (typep a 'optimal-2q-target-atom))
              seq)))

(deftype optimal-2q-target ()
  "A valid TARGET value for OPTIMAL-2Q-COMPILE."
  '(or optimal-2q-target-atom
    (and sequence
     (satisfies sequence-of-optimal-2q-target-atoms-p))))

(defun optimal-2q-target-meets-requirements (target requirements)
  "Tests whether REQUIREMENTS of type OPTIMAL-2Q-TARGET, thought of as the two-qubit instructions necessary to instantiate a two-qubit decomposition template, is covered by TARGET of type OPTIMAL-2Q-TARGET, thought of as the available two-qubit instructions on the device."
  (let ((targetl       (a:ensure-list target))
        (requirementsl (a:ensure-list requirements)))
    (when (member ':cphase targetl) (push ':cz targetl))
    (when (member ':piswap targetl) (push ':iswap targetl))
    (when (member ':xy targetl) (push ':iswap targetl))
    (when (member ':cnot targetl) (push ':cz targetl))
    (subsetp requirementsl targetl)))

(defun gate-application-trivially-satisfies-2q-target-requirements (instr requirements)
  "Does the gate application INSTR trivially satisfy the requirements imposed by REQUIREMENTS? (In other words, do we actually need to do decomposition?)"
  (check-type instr gate-application)
  (and (plain-operator-p (application-operator instr))
       (let ((name (application-operator-name instr)))
         (flet ((good (req)
                  (case req
                    (:cz       (string= name "CZ"))
                    (:iswap    (string= name "ISWAP"))
                    (:piswap   (string= name "PISWAP"))
                    (:xy       (string= name "XY"))
                    (:cphase   (string= name "CPHASE"))
                    (otherwise nil))))
           (some #'good requirements)))))


;;; we do some very hands-on linear algebra far below. these next routines are
;;; all in support for that.

(defun convert-su4-to-su2x2 (SU2x2)
  "Assuming SU2x2 is in the subgroup SU(2) (x) SU(2) of SU(4), this computes the parent matrices."
  (check-type SU2x2 magicl:matrix)
  ;; the algorithm presented here was cooked up by van Loan and Pitsianis in
  ;; _Approximation with Kronecker Products_. the good news is that, because it
  ;; sits atop SVD, it has pretty good numerical stability properties.
  ;; previously, we were picking the term with the largest magnitude in the
  ;; first column and using it to brute-force a factorization, but the term with
  ;; the largest magnitude may also have the largest magnitude of jitter. this
  ;; eventually bit us. the bad news is that, even though it provably constructs
  ;; the best approximation to a kronecker product, but the factors in the best
  ;; approximation may not, in general, be unitary. this is a remark in the
  ;; original paper, toward the very end, and i honestly don't understand it:
  ;; i think that in the one-summand case, it's safe to ignore this caveat.
  ;;
  ;; NOTE: this logic extends just as well to SU(m) (x) SU(n) and could arguably
  ;;       be moved into MAGICL.
  (let ((m (zeros '(4 4))))
    (dotimes (block-i 2)
      (dotimes (block-j 2)
        (dotimes (inner-i 2)
          (dotimes (inner-j 2)
            (setf (magicl:tref m (+ (* 2 block-j) block-i) (+ (* 2 inner-j) inner-i))
                  (magicl:tref SU2x2 (+ (* 2 block-i) inner-i) (+ (* 2 block-j) inner-j)))))))
    (multiple-value-bind (u sigma vt) (magicl:svd m)
      (let ((a (zeros '(2 2)))
            (b (zeros '(2 2))))
        (dotimes (i 2)
          (dotimes (j 2)
            (setf (magicl:tref a i j)
                  (magicl:tref u (+ (* 2 j) i) 0))))
        (setf a (magicl:scale a (sqrt (magicl:tref sigma 0 0))))
        (dotimes (i 2)
          (dotimes (j 2)
            (setf (magicl:tref b i j)
                  (magicl:tref vt 0 (+ (* 2 j) i)))))
        (setf b (magicl:scale b (sqrt (magicl:tref sigma 0 0))))
        (values a b)))))

;; these are special matrices that conjugate SU(2) x SU(2) onto SO(4).
;;
;; REM: according to arXiv:0308033, their only special property is:
;;     e e^T = -(sigma_y (x) sigma_y) .
;; there are several such matrices; this one is just easy to write down.
(a:define-constant
    +e-basis+
    (let* ((sqrt2 (/ (sqrt 2d0) 2))
           (-sqrt2 (- sqrt2))
           (isqrt2 (complex 0 sqrt2))
           (-isqrt2 (complex 0 -sqrt2)))
      (from-list (list sqrt2  isqrt2  0       0
                       0      0       isqrt2  sqrt2
                       0      0       isqrt2 -sqrt2
                       sqrt2 -isqrt2  0       0)
                 '(4 4)))
  :test #'magicl:=
  :documentation "This is an element of SU(4) that has two properties: (1) e-basis e-basis^T = - sigma_y^((x) 2) and (2) e-basis^dag SU(2)^((x) 2) e-basis = SO(4).")

(a:define-constant
    +edag-basis+
    (let* ((sqrt2 (/ (sqrt 2d0) 2))
           (-sqrt2 (- sqrt2))
           (isqrt2 (complex 0 sqrt2))
           (-isqrt2 (complex 0 -sqrt2)))
      (from-list (list sqrt2   0       0      sqrt2
                       -isqrt2  0       0      isqrt2
                       0      -isqrt2 -isqrt2 0
                       0       sqrt2  -sqrt2  0)
                 '(4 4)))
  :test #'magicl:=
  :documentation "This is a precomputed Hermitian transpose of +E-BASIS+.")

(defun ensure-positive-determinant (m)
  (let ((d (magicl:det m)))
    (when *check-math*
      (unless (double~ 0.0d0 (imagpart d))
        (warn "Complex determinant found for a ~
               matrix expected to be (real) orthogonal: ~
                   det=~A" d)))
    (if (double= -1d0 (realpart d))
        (magicl:@ m (from-diag (list -1 1 1 1)))
        m)))

(defun orthogonalp (m)
  "Does M appear to be orthogonal?"
  (magicl:identity-matrix-p
   (magicl:@ m (magicl:transpose m))
   +double-comparison-threshold-loose+))

(defun diagonal-matrix-p (m)
  (dotimes (i (magicl:nrows m) t)
    (dotimes (j (magicl:ncols m))
      (when (and (/= i j)
                 (not (double~ 0.0d0 (magicl:tref m i j))))
        (return-from diagonal-matrix-p nil)))))

(defun zero-matrix-p (m)
  (dotimes (i (magicl:nrows m) t)
    (dotimes (j (magicl:ncols m))
      (when (not (double~ 0.0d0 (abs (magicl:tref m i j))))
        (return-from zero-matrix-p nil)))))

(defun permuted-diagonal-matrix-p (m)
  (flet ((make-permutation-matrix (perm)
           (loop :with p := (magicl:zeros (magicl:shape m) :type (magicl:element-type m))
                 :for i :below (magicl:nrows m)
                 :for p_i :in perm
                 :do (incf (magicl:tref p i p_i))
                 :finally (return p))))
    (let ((permutation nil))
      (dotimes (i (magicl:nrows m) (make-permutation-matrix
                                    (nreverse permutation)))
        (let ((found? nil))
          (dotimes (j (magicl:ncols m))
            (cond
              ((not (double~ 0.0d0 (magicl:tref m i j)))
               (when found?
                 (return-from permuted-diagonal-matrix-p nil))
               (setf found? t)
               (push j permutation)))))))))

(defun real->complex (m)
  "Convert a real matrix M to a complex one."
  (let ((cm (magicl:zeros
             (magicl:shape m)
             :type `(complex ,(magicl:element-type m)))))
    (magicl::map-to #'complex m cm)
    cm))

(defun diagonalize-uu^t (u)
  "Given a unitary U, produce an X such that

    X^T Re[UU^T] X,    X^T Im[UU^T] X

are diagonal. Return (VALUES X UU^T).
"
  (let* ((uut (magicl:@ u (magicl:transpose u)))
         (a (magicl:map #'realpart uut))
         (b (magicl:map #'imagpart uut)))
    (cond
      ((or (zero-matrix-p a)
           (permuted-diagonal-matrix-p a))
       (values (nth-value 1 (magicl:eig b))
               uut))
      ((or (zero-matrix-p b)
           (permuted-diagonal-matrix-p b))
       (values (nth-value 1 (magicl:eig a))
               uut))
      (t
       (multiple-value-bind (_ g) (magicl:eig b)
         (declare (ignore _))
         (let* ((g-inv (magicl:inv g))
                (g-inv-transpose (magicl:transpose g-inv))
                (c (magicl:@ g-inv a g-inv-transpose)))
           (multiple-value-bind (_ v) (magicl:eig c)
             (declare (ignore _))
             (values (magicl:@ g-inv-transpose (magicl:transpose v))
                     uut))))))))

(defun find-diagonalizer-in-e-basis (m)
  "For M in SU(4), compute an SO(4) column matrix of eigenvectors of E^* M E (E^* M E)^T."
  (check-type m magicl:matrix)
  (assert (magicl:unitary-matrix-p m))
  (let ((u (magicl:@ +edag-basis+ m +e-basis+)))
    (multiple-value-bind (evecs gammag) (diagonalize-uu^t u)
      (setf evecs (ensure-positive-determinant (orthonormalize-matrix! evecs)))
      (when *check-math*
        (assert (double= 1.0d0 (magicl:det evecs)))
        (assert (diagonal-matrix-p (magicl:@ (magicl:transpose evecs)
                                             gammag
                                             evecs))
            ()
            "X^T (UU^T) X not diagonal!

X =
~A

U =
~A

UU^T =
~A

X^T(UU^T)X =
~A

Original M such that U = E^T M E is
~A"
            evecs
            u
            gammag
            (magicl:@ (magicl:transpose evecs)
                                             gammag
                                             evecs)
            m)
        (assert (magicl:every #'double~
                              (eye 4 :type 'double-float)
                              (magicl:@ (magicl:transpose evecs)
                                        evecs))
            (evecs)
            "The calculated eigenvectors were not found to be orthonormal. ~
               EE^T =~%~A"
            (magicl:@ (magicl:transpose evecs)
                      evecs)))
      evecs)))

(defun diagonalizer-in-e-basis (m)
  "For M in SU(4), compute an SO(4) column matrix of eigenvectors of E^* M E (E^* M E)^T."
  (find-diagonalizer-in-e-basis m))

(defun orthogonal-decomposition (m)
  "Extracts from M a decomposition of E^* M E into A * D * B, where A and B are orthogonal and D is diagonal.  Returns the results as the VALUES triple (VALUES A D B)."
  (let* ((m (magicl:scale m (expt (magicl:det m) -1/4)))
         (a (diagonalizer-in-e-basis m))
         (db (magicl:@ (magicl:transpose a) +edag-basis+ m +e-basis+))
         (diag (loop :for j :below 4
                     :collect (let ((mag 0d0)
                                    phase)
                                (dotimes (i 4)
                                  (when (>= (abs (magicl:tref db j i)) mag)
                                    (setf mag (abs (magicl:tref db j i)))
                                    (setf phase (mod (phase (magicl:tref db j i)) pi))))
                                (cis phase))))
         (d (from-diag diag))
         (b (magicl:@ (magicl:conjugate-transpose d) db)))
    ;; it could be the case that b has negative determinant. if that's
    ;; the case, we'll swap two of its columns that live in the same
    ;; eigenspace.  We want to preserve the equation M = ADB and D's
    ;; diagonal form, so in our scheme to insert an orthogonal matrix
    ;; O like M = A(DO)(O^T B), we need to pick O so that (1) O^T B
    ;; has determinant 1 and (2) DO is again diagonal. The second
    ;; condition excludes permutation matrices. - ecp
    (when (double~ -1d0 (magicl:det b))
      (setf d (magicl:@ d (from-diag (list -1 1 1 1))))
      (setf b (magicl:@ (from-diag (list -1 1 1 1)) b)))
    (when *compress-carefully*
      (assert (double~ 1d0 (magicl:det m)))
      (assert (double~ 1d0 (magicl:det a)))
      (assert (double~ 1d0 (magicl:det b)))
      (assert (double~ 1d0 (magicl:det d)))
      (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                  (magicl:@ a (magicl:transpose a))))
      (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                  (magicl:@ b (magicl:transpose b))))
      (assert (matrix-equals-dwim (magicl:@ +edag-basis+ m +e-basis+)
                                  (magicl:@ a d b))))
    (values a d b)))

(defun make-signed-permutation-matrix (sigma &optional (signs (list 1 1 1 1)))
  (let ((o (zeros '(4 4))))
    (loop :for i :below 4
          :for j := (1- (cl-permutation:perm-eval sigma (1+ i)))
          :for sign :in signs
          :do (setf (magicl:tref o i j) sign))
    o))

(defun match-matrix-to-an-e-basis-diagonalization (mprime a d b)
  "Given a matrix MPRIME and a decomposed matrix E^* M E = A D B, this computes matrices UA and UB so that UA MPRIME UB gives the best approximation to M = E A D B E^*.  Returns UA and UB as the values pair (VALUES UA UB)."
  ;; start by decomposing e^* m' e = a' d' b'.
  ;; we then maximize over signed permutation matrices o so that
  ;;     d' ~~ o d o^T
  ;; is as good an approximation as possible.
  ;; then, naming m = e (a d b) e^*, it follows that
  ;;     m  = e (a d b) e^*
  ;;       ~~ e a o^T d' o b e^*
  ;;        = e a o^T a'^T a' d' b' b'^T o b e^*
  ;;        = (e a o^T a'^T e^*) (e a' d' b' e^*) (e b'^T o b e^*)
  ;;        = (e a o^T a'^T e^*) m (e b'^T o b e^*)
  ;; is as good an approximation as possible.
  ;; we return the SU(2) (x) SU(2) versions of the triple products, as on the
  ;; last line, as well as the fidelity as a values triple.
  (multiple-value-bind (aprime dprime bprime)
      (orthogonal-decomposition (magicl:scale mprime (expt (magicl:det mprime) -1/4)))
    (let (o
          oT
          (d-as-list (magicl:diag d))
          (dprime-as-list (magicl:diag dprime))
          (max-fidelity 0d0))
      ;; maximize the trace over signed permutations
      (cl-permutation:doperms (sigma 4)
        (dolist (signs (list (list  1  1  1  1)
                             (list -1 -1  1  1)
                             (list -1  1 -1  1)
                             (list -1  1  1 -1)))
          (let* ((new-trace
                   (loop
                     :for x :in (cl-permutation:permute sigma d-as-list)
                     :for y :in dprime-as-list
                     :for sign :in signs
                     :sum (* x sign (conjugate y))))
                 (new-fidelity (/ (+ 4 (abs (* new-trace new-trace))) 20)))
            (when (> new-fidelity max-fidelity)
              (setf max-fidelity new-fidelity)
              (when (= -1 (cl-permutation:perm-sign sigma))
                (setf (first signs) (- (first signs))))
              (setf o (make-signed-permutation-matrix sigma (if (= -1 (cl-permutation:perm-sign sigma))
                                                                (list -1 1 1 1)
                                                                (list 1 1 1 1))))
              (setf oT (magicl:transpose (make-signed-permutation-matrix sigma signs)))))))
      (when *compress-carefully*
        (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                    (magicl:@ a (magicl:transpose a))))
        (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                    (magicl:@ b (magicl:transpose b))))
        (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                    (magicl:@ aprime (magicl:transpose aprime))))
        (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                    (magicl:@ bprime (magicl:transpose bprime))))
        (assert (matrix-equals-dwim (from-diag '(1d0 1d0 1d0 1d0))
                                    (magicl:@ o (magicl:transpose o))))
        (assert (double~ 1d0 (magicl:det a)))
        (assert (double~ 1d0 (magicl:det aprime)))
        (assert (double~ 1d0 (magicl:det b)))
        (assert (double~ 1d0 (magicl:det bprime)))
        (assert (double~ 1d0 (magicl:det o))))
      (values (reduce #'magicl:@
                      (list +e-basis+ a oT (magicl:transpose aprime) +edag-basis+))
              (reduce #'magicl:@
                      (list +e-basis+ (magicl:transpose bprime) o b +edag-basis+))
              max-fidelity))))


;;; we also provide some utilities related specifically to canonical gates and
;;; to trace distance.

(defun trace-distance (m1 m2)
  "Calculates the average fidelity distance between two unitary operators M1 and M2.  A by-the-book definition of this function is that for operators M1 and M2 acting on a vector space V, we want to calculate

     (trace-distance m1 m2) = \\int_{psi in P(V)} <psi| M1^* M2 |psi>.

One can show (cf., e.g., the formulas in arXiv:0205035 with U = M2, E(rho) = V rho V^*) that this is equal to the trace calculation that's actually used in this implementation."
  (assert (= (magicl:nrows m1) (magicl:ncols m1)
             (magicl:nrows m2) (magicl:ncols m2)))
  (let* ((n (magicl:nrows m1))
         (prod (magicl:@ m1 (magicl:conjugate-transpose m2)))
         (tr (magicl:trace prod)))
    (/ (+ n (abs (* tr tr)))
       (+ n (* n n)))))

(defun fidelity-coord-distance (coord1 coord2)
  "Calculates the average fidelity distance between the canonical gates associated to the canonical coordinates COORD1 and COORD2."
  (let* ((n 4)
         (delta-a (/ (- (first coord1)  (first coord2))  2))
         (delta-b (/ (- (second coord1) (second coord2)) 2))
         (delta-c (/ (- (third coord1)  (third coord2))  2))
         (tr (complex (* (cos delta-a) (cos delta-b) (cos delta-c))
                      (* (sin delta-a) (sin delta-b) (sin delta-c)))))
    (- 1 (/ (+ n (abs (* 16 tr tr)))
            (+ n (* n n))))))

(defun fidelity-of-straight-quil (instrs chip-spec)
  "Helper routine for calculating the fidelity of a straight line of Quil instructions against the fidelity information associated to CHIP-SPEC."
  (let ((ls (make-lscheduler)))
    (append-instructions-to-lschedule ls instrs)
    (lscheduler-calculate-fidelity ls chip-spec)))

(defun get-canonical-coords-from-diagonal (d)
  "Extracts \"canonical coordinates\" (c1, c2, c3) from a diagonal matrix D which belong to the Weyl chamber satisfying

    pi/2 >= c1 >= c2 >= |c3|."
  (assert (= 4 (magicl:nrows d) (magicl:ncols d)))
  (labels ((test (seq) (double>= pi/2 (first seq) (second seq) (abs (third seq))))
           (wrap-value (z)
             (let ((z (- (mod (+ z pi/2) pi) pi/2)))
               (if (double= -pi/2 z) (- z) z)))
           (try-to-canonicalize (a b c)
             (let ((intermediate-value (sort (mapcar #'wrap-value (list a b c)) #'>)))
               (cond
                 ((member 0d0 intermediate-value :test #'double=)
                  (sort (mapcar #'abs intermediate-value) #'>))
                 ((member pi/2 intermediate-value :test #'double=)
                  (sort (mapcar #'abs intermediate-value) #'>))
                 (t intermediate-value)))))
    (let* ((angles (mapcar #'phase (magicl:diag d)))
           (first  (mod    (+ (third angles) (fourth angles)) pi))
           (second (mod (- (+ (third angles) (first angles))) pi))
           (third  (mod    (+ (third angles) (second angles)) pi))
           (option-1 (try-to-canonicalize first second third)))
      (destructuring-bind (first second third) option-1
        (let ((option-2 (try-to-canonicalize (- first) (- second) third))
              (option-3 (try-to-canonicalize (- first) second     (- third)))
              (option-4 (try-to-canonicalize first     (- second) (- third))))
          (cond
            ((test option-1) option-1)
            ((test option-2) option-2)
            ((test option-3) option-3)
            ((test option-4) option-4)
            (t (error "Failed to put the canonical coordinates ~A into the preferred Weyl chamber." (list first second third)))))))))

(defun build-canonical-gate-in-magic-basis (coord)
  "Given a canonical coordinate, construct the associated canonical gate at that coordinate."
  (destructuring-bind (c1 c2 c3) coord
    (from-diag
     (mapcar (lambda (z) (cis (* 0.5d0 z)))
             (list (+    c1  (- c2)    c3)
                   (+    c1     c2  (- c3))
                   (+ (- c1) (- c2) (- c3))
                   (+ (- c1)    c2     c3))))))

(defun sandwich-with-local-gates (center-circuit a d b q1 q0)
  "Given a circuit CENTER-CIRCUIT and an E-basis-diagonalized operator (A, D, B) (as returned by ORTHOGONAL-DECOMPOSITION), this routine computes an extension of CENTER-CIRCUIT by local gates which maximizes the trace fidelity with the product (E-BASIS)ADB(EDAG-BASIS).

Both CENTER-CIRCUIT and the return value are lists of GATE-APPLICATIONs; A, D, and B are matrices; and Q1, Q0 are qubit indices."
  (multiple-value-bind (ua ub fidelity)
      (match-matrix-to-an-e-basis-diagonalization
       (make-matrix-from-quil center-circuit :relabeling (standard-qubit-relabeler `(,q1 ,q0)))
       a d b)

    (multiple-value-bind (b1 b0) (convert-su4-to-su2x2 ub)
      (multiple-value-bind (a1 a0) (convert-su4-to-su2x2 ua)
        (values
         (append (list (anon-gate "B0" b0 q0)
                       (anon-gate "B1" b1 q1))
                 center-circuit
                 (list (anon-gate "A0" a0 q0)
                       (anon-gate "A1" a1 q1)))
         fidelity)))))


;;; now, finally, we start writing approximation-specific things.  what follows
;;; are templates that plug into the general framework of approximate compilation.
;;; these are accreted into the global **approximate-template-records**, which
;;; will need flushing if you ever need to modify a particular template.
;;; otherwise, this list of records will retain both the record pointing to the
;;; new definition as well as the previously existing record pointing to the old.
;;;
;;; IMPORTANT NOTE: these are listed in *descending* preference order.

(global-vars:define-global-var **approximate-template-records** nil
  "Houses a list of available approximate templates, sorted in *descending* preference order.")

(defstruct approximate-template-record
  (name nil :type symbol :read-only t)
  predicate
  requirements)

(defvar *approximate-template-search-limit* 5000
  "Tolerance level for how many guesses an inexact template solver is allowed to make when it is unsure that an exact solution exists.")

(defclass approximate-compiler (compiler)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A breed of COMPILER that has the potential to emit inexact decompositions when the flag *ENABLE-APPROXIMATE-COMPILATION* is set."))

(defmacro define-canonical-circuit-approximation (name (&rest bindings) &body body)
  "This defines an two-qubit circuit template for use by the approximation algorithm. The template is stored both as a raw function under the name NAME as well as in a list of available templates, guarded by the value REQUIREMENTS of type OPTIMAL-2Q-TARGET.  BODY is a routine that returns a list of GATE-APPLICATION objects and is allowed to reference three arguments: the desired canonical coordinate COORD, the first desired qubit Q1, and the zeroth desired qubit Q0.

Additionally, if PREDICATE evaluates to false and *ENABLE-APPROXIMATE-COMPILATION* is NIL, the template errors with GIVE-UP-COMPILATION and does not evaluate BODY; this mechanism is used to avoid the emission of approximate templates when they aren't wanted.  If a template author cannot provide a meaningful predicate, they must manually install a guard against such unwanted emissions."
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (a:with-gensyms (circuit coord q0 q1)
      (let ((instr-name (if (typep (first bindings) 'symbol)
                            (first bindings)
                            (first (first bindings)))))
        `(progn
           (define-compiler ,name (,@bindings
                                   :class approximate-compiler
                                   :permit-binding-mismatches-when *enable-approximate-compilation*)
             ,docstring
             ,@decls
             (let ((,circuit (with-inst () ,@body))
                   (,coord (mapcar #'constant-value (application-parameters ,instr-name)))
                   (,q1 (qubit-index (first (application-arguments ,instr-name))))
                   (,q0 (qubit-index (second (application-arguments ,instr-name)))))
               (multiple-value-bind (complete-circuit fidelity)
                   (sandwich-with-local-gates ,circuit
                                              (from-diag '(1d0 1d0 1d0 1d0))
                                              (build-canonical-gate-in-magic-basis ,coord)
                                              (from-diag '(1d0 1d0 1d0 1d0))
                                              ,q1 ,q0)
                 (finish-compiler (values complete-circuit fidelity))))))))))

(defmacro define-searching-approximate-template (name (coord q1 q0 parameter-array)
                                                 (&key predicate
                                                    parameter-count)
                                                 &body parametric-circuit)
  "Defines an approximate template that uses an inexact (and possibly imperfect) search algorithm (e.g., a Nelder-Mead solver).  In addition to the documentation of DEFINE-CANONICAL-CIRCUIT-APPROXIMATION, this macro takes the extra value PARAMETER-COUNT which controls how many variables the searcher will optimize over."
  (a:with-gensyms (instr a d b in goodness template-values)
    (multiple-value-bind (parametric-circuit decls docstring)
        (alexandria:parse-body parametric-circuit :documentation t)
      `(define-canonical-circuit-approximation ,name
           ((,instr ("CAN" ,coord ,q1 ,q0)
                    ;; this is here to throw the compiler hunter off the scent
                    :where t))
         ,@(when docstring (list docstring))
         ,@decls
         (labels
             ((circuit-template (,parameter-array ,q1 ,q0)
                (with-inst ()
                  ,@parametric-circuit))
              (run-optimizer ()
                (multiple-value-bind (,template-values ,goodness)
                    (cl-grnm:nm-optimize
                     (lambda (,in)
                       (multiple-value-bind (,a ,d ,b)
                           (orthogonal-decomposition (make-matrix-from-quil (circuit-template ,in 1 0)))
                         (declare (ignore ,a ,b))
                         (fidelity-coord-distance ,coord (get-canonical-coords-from-diagonal ,d))))
                     (make-array ,parameter-count
                                 :initial-contents (mapcar #'random
                                                           (make-list ,parameter-count
                                                                      :initial-element 2pi)))
                     :max-function-calls *approximate-template-search-limit*)
                  (cond
                    ;; if we promised an exact solution but haven't found it yet,
                    ;; try again.
                    ((and (not (double= 0d0 ,goodness))
                          ,predicate)
                     (run-optimizer))
                    ;; if we are unsure about the existence of an exact solution, we
                    ;; haven't found one yet, but the user is demanding one, give up.
                    ((and (not *enable-approximate-compilation*)
                          (not (double= 0d0 ,goodness)))
                     (give-up-compilation))
                    ;; otherwise, this solution will do.
                    (t
                     (dolist (instr (circuit-template ,template-values ,q1 ,q0))
                       (inst instr)))))))
           (run-optimizer))))))


(define-canonical-circuit-approximation nearest-circuit-of-depth-0
    ((instr ("CAN" (0 0 0) q1 q0)))
  "Produces a decomposition of the canonical gate using zero two-qubit operations."
  (inst "I" () q0)
  (inst "I" () q1))

(define-canonical-circuit-approximation nearest-ISWAP-circuit-of-depth-1
    ((instr ("CAN" (#.pi/2 #.pi/2 0) q1 q0)))
  (inst "ISWAP" () q1 q0))

(define-canonical-circuit-approximation nearest-XY-circuit-of-depth-1
    ((instr ("CAN" (alpha alpha 0) q1 q0)))
  (inst "PISWAP" (list (* 2 alpha)) q1 q0))

(define-canonical-circuit-approximation nearest-CZ-circuit-of-depth-1
    ((instr ("CAN" (#.pi/2 0 0) q1 q0)))
  (inst "CZ" () q1 q0))

(define-canonical-circuit-approximation nearest-CPHASE-circuit-of-depth-1
    ((instr ("CAN" (alpha 0 0) q1 q0)))
  (inst "CPHASE" (list (* 2 alpha)) q1 q0))

(define-canonical-circuit-approximation nearest-ISWAP-circuit-of-depth-2
    ((instr ("CAN" (alpha beta 0) q1 q0)))
  (inst "ISWAP" ()           q1 q0)
  (inst "RY"    (list alpha) q1)
  (inst "RY"    (list beta)  q0)
  (inst "ISWAP" ()           q1 q0))

(define-searching-approximate-template nearest-XY-XY-template-of-depth-2 (coord q1 q0 array)
    (:predicate nil                     ; TODO: replace this with a convexity test
     :parameter-count 6)
  (inst "PISWAP" (list (aref array 4))     q1 q0)
  (inst "RZ"     (list (aref array 5))     q0)
  (inst "RZ"     (list (- (aref array 5))) q1)
  (inst "RY"     (list (aref array 0))     q0)
  (inst "RY"     (list (aref array 1))     q1)
  (inst "RZ"     (list (aref array 2))     q0)
  (inst "RZ"     (list (- (aref array 2))) q1)
  (inst "PISWAP" (list (aref array 3))     q1 q0))

(define-canonical-circuit-approximation nearest-CZ-ISWAP-circuit-of-depth-2
    ((instr ("CAN" (#.pi/2 beta gamma) q1 q0)))
  (inst "ISWAP" () q1 q0)
  (inst "RY"    (list (- pi/2 beta))  q0)
  (inst "RY"    (list (- pi/2 gamma)) q1)
  (inst "CZ"    () q1 q0))

(define-canonical-circuit-approximation nearest-ISWAP-circuit-of-depth-3
    ((instr ("CAN" (_ _ _) q1 q0)))
  (flet ((twist-to-real (m)
           ;; this magical formula was furnished to us by asking a CAS to compute
           ;; the trace of M' for a symbolic M and SIGMA, then solving
           ;;     0 = imagpart(tr) = imagpart(a cos(sigma) + b sin(sigma)).
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
                       (gate-matrix (gate-definition-to-gate (lookup-standard-gate "ISWAP")))))))))
    (multiple-value-bind (sigma mprime) (twist-to-real (gate-matrix instr))
      (multiple-value-bind (a d b) (orthogonal-decomposition mprime)
        (destructuring-bind (alpha beta gamma) (get-canonical-coords-from-diagonal d)
          (declare (ignore gamma))
          (inst "RY"    (list (- sigma)) q0)
          (inst "ISWAP" ()              q1 q0)
          (inst "Z"     ()              q0)
          (inst "Z"     ()              q1)
          (dolist (instr
                   (sandwich-with-local-gates
                    (with-inst ()
                      (inst "ISWAP" '()       q1 q0)
                      (inst "RY"    `(,alpha) q1)
                      (inst "RY"    `(,beta)  q0)
                      (inst "ISWAP" '()       q1 q0))
                    a d b q1 q0))
            (inst instr)))))))

(define-searching-approximate-template nearest-CZ-XY-template-of-depth-2 (coord q1 q0 array)
    (:predicate nil   ; TODO: replace this with a convexity test
     :parameter-count 4)
  (inst "CZ"     ()                        q1 q0)
  (inst "RY"     (list (aref array 0))     q0)
  (inst "RY"     (list (aref array 1))     q1)
  (inst "RZ"     (list (aref array 2))     q0)
  (inst "RZ"     (list (- (aref array 2))) q1)
  (inst "PISWAP" (list (aref array 3))     q1 q0))

(define-searching-approximate-template nearest-CPHASE-XY-template-of-depth-2 (coord q1 q0 array)
    (:predicate nil                     ; TODO: replace this with a convexity test
     :parameter-count 5)
  (inst "CPHASE" (list (aref array 4))     q1 q0)
  (inst "RY"     (list (aref array 0))     q0)
  (inst "RY"     (list (aref array 1))     q1)
  (inst "RZ"     (list (aref array 2))     q0)
  (inst "RZ"     (list (- (aref array 2))) q1)
  (inst "PISWAP" (list (aref array 3))     q1 q0))

(define-canonical-circuit-approximation nearest-CZ-circuit-of-depth-2
    ((instr ("CAN" (alpha beta 0d0) q1 q0)))
  (inst "CZ" () q1 q0)
  (inst "RY" (list alpha) q1)
  (inst "RY" (list beta) q0)
  (inst "CZ" () q1 q0))

(define-canonical-circuit-approximation nearest-CZ-circuit-of-depth-3
    ((instr ("CAN" (alpha beta gamma) q1 q0)))
  (let ((a (- alpha pi))
        (b (- pi    beta))
        (c (- pi/2  gamma)))
    (inst "CZ"  ()        q0 q1)
    (inst "RY" '(#.-pi/2) q0)
    (inst "RY"  (list b)  q1)
    (inst "RZ"  (list c)  q0)
    (inst "CZ"  ()        q0 q1)
    (inst "RY"  (list a)  q1)
    (inst "RY" '(#.pi/2)  q0)
    (inst "CZ"  ()        q0 q1)))


;;; here lies the logic underlying the approximate compilation routine.

(define-compiler canonical-decomposition
    ((instr (_ _ q1 q0)))
  (handler-case
      (let* ((m (or (gate-matrix instr) (give-up-compilation :because ':invalid-domain)))
             (m (magicl:scale m (expt (magicl:det m) -1/4))))
        (multiple-value-bind (a d b) (orthogonal-decomposition m)
          (let ((canonical-coords (get-canonical-coords-from-diagonal d)))
            (destructuring-bind (b0 b1 center-circuit a0 a1)
                (sandwich-with-local-gates (list (build-gate "CAN" canonical-coords q1 q0))
                                           a d b q1 q0)
              (declare (ignore center-circuit))
              (inst "B0"  (gate-matrix b0) q0)
              (inst "B1"  (gate-matrix b1) q1)
              (inst "CAN" canonical-coords q1 q0)
              (inst "A0"  (gate-matrix a0) q0)
              (inst "A1"  (gate-matrix a1) q1)))))
    (unknown-gate-parameter ()
      (give-up-compilation :because ':invalid-domain))))

(defun approximate-2Q-compiler (crafters instr &key context)
  "Generic logic for performing (approximate) two-qubit compilation.  This consumes an instruction INSTR to compile, an optional CHIP-SPEC of type CHIP-SPECIFICATION which records fidelity information, and a list of circuit template manufacturers CRAFTERS to run through.

NOTE: This routine degenerates to an optimal 2Q compiler when *ENABLE-APPROXIMATE-COMPILER* is NIL."
  (check-type instr gate-application)
  (check-type context compilation-context)

  (unless (= 2 (length (application-arguments instr)))
    (give-up-compilation))

  ;; extract matrix, canonical decomposition
  (destructuring-bind (left1 left2 can right1 right2) (canonical-decomposition instr)
    (let ((q1 (qubit-index (first (application-arguments instr))))
          (q0 (qubit-index (second (application-arguments instr))))
          (candidate-pairs nil)
          (chip-spec (compilation-context-chip-specification context)))

      ;; now we manufacture a bunch of candidate circuits
      (dolist (circuit-crafter crafters)
        (unless (and (first candidate-pairs)
                     (double= 1d0 (car (first candidate-pairs))))
          (format-noise
           "~&APPROXIMATE-2Q-COMPILER: Trying ~A on ~A..."
           circuit-crafter
           (with-output-to-string (s) (print-instruction instr s)))
          (handler-case
              (let* ((center-circuit (funcall circuit-crafter can))
                     (ls (append-instructions-to-lschedule (make-lscheduler) center-circuit))
                     (circuit-cost (or (and chip-spec (lscheduler-calculate-fidelity ls chip-spec))
                                       1d0))
                     (sandwiched-circuit (append (list left1 left2)
                                                 center-circuit
                                                 (list right1 right2)))
                     (m (make-matrix-from-quil sandwiched-circuit
                                               :relabeling (standard-qubit-relabeler `(,q1 ,q0)))))
                (let ((infidelity (fidelity-coord-distance
                                   (mapcar #'constant-value (application-parameters can))
                                   (get-canonical-coords-from-diagonal
                                    (nth-value 1 (orthogonal-decomposition m))))))
                  (format-noise " for infidelity ~A." infidelity)
                  (push (cons (* circuit-cost (- 1 infidelity)) sandwiched-circuit)
                        candidate-pairs)))
            (compiler-does-not-apply () nil))))

      ;; now vomit the results
      (when (endp candidate-pairs)
        (give-up-compilation))
      (destructuring-bind (fidelity . circuit) (a:extremum candidate-pairs #'> :key #'car)
        (unless (or *enable-approximate-compilation*
                    (double= 1d0 fidelity))
          (give-up-compilation))
        (values circuit fidelity)))))
