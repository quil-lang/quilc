;;;; optimal-2q.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)

(define-global-counter **optimal-2q-twist-counter** generate-optimal-2q-twist-tag)

(defconstant +makhlin-distance-to-operator-distance-postfactor+ 4
  "In the invocations of Nelder-Mead in the routine OPTIMAL-2Q-COMPILE below, GOODNESS is a measure of the L^2-distance between the vectors of Makhlin invariants of the input 2Q operator and the output of the Nelder-Mead finder for the circuit template.  This is a measure of distance between the two double-cosets, which is not an exact reflection of the L^2-distance between the two operators.  In fact, it's not possible to measure that distance until the rest of the routine has completed, which picks out a particular representative element of the coset carved out by the chosen template.  Nonetheless, we have to make a decision earlier as to whether a given template is appropriate.

NOTE: I believe that even though both objects (the double-coset space and the space of projective unitaries) are compact, it actually does not suffice to scale up the distance by a constant factor (for some small fraction of gates): the Makhlin map has a degenerate derivative at the edge of the figure, so that 1/det D goes to infinity.  Bump this value up if the tests ever start failing.  A more long-lasting solution might be to work with alcove coordinates instead of Makhlin coordinates, which do not suffer from this degeneracy.")


;; optimal-2Q-compile returns the smallest string of 2Q gates that it can, but
;; what this means depends upon what gateset is available. the TARGET argument
;; is either an OPTIMAL-2Q-TARGET-ATOM or a(n unsorted) sequence of such atoms,
;; indicating which 2Q gates are available for use.
(deftype optimal-2q-target-atom ()
  '(member :cz :iswap :piswap :cphase :cnot))

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
  (let ((targetl       (alexandria:ensure-list target))
        (requirementsl (alexandria:ensure-list requirements)))
    (when (member ':cphase targetl) (push ':cz targetl))
    (when (member ':piswap targetl) (push ':iswap targetl))
    (when (member ':cnot targetl) (push ':cz targetl))
    (subsetp requirementsl targetl)))

(defun convert-su4-to-su2x2 (m)
  "Assuming m is in the subgroup SU(2) x SU(2) of SU(4), this computes the parent matrices."
  (check-type m magicl:matrix)
  ;; we assume that we're looking at a matrix of the form
  ;; [ a00 b00, a00 b01, a01 b00, a01 b01;
  ;;   a00 b10, a00 b11, a01 b10, a01 b11;
  ;;   a10 b00, a10 b01, a11 b00, a11 b01;
  ;;   a10 b10, a10 b11, a11 b10, a11 b11 ] .
  ;; the goal is to extract the values aij and bij.
  (let* (;; there are four cases in all, depending on which of the entries in
         ;; the first column are nonzero. (because the entire matrix is unitary,
         ;; we know that *at least one* entry is nonzero.)
         (state (alexandria:extremum (alexandria:iota 4) #'>
                                     :key (lambda (i) (abs (magicl:ref m 0 i)))))
         ;; b can be taken to be either the UL block or the LL block, up to
         ;; rescaling, by assigning the first nonzero value in the first column
         ;; as belonging to a
         (b
           (magicl:scale (/ 1 (magicl:ref m 0 state))
                         (cond
                           ((or (= state 0) (= state 1))
                            (magicl:make-complex-matrix 2 2
                                                        (list (magicl:ref m 0 0) (magicl:ref m 1 0)
                                                              (magicl:ref m 0 1) (magicl:ref m 1 1))))
                           ((or (= state 2) (= state 3))
                            (magicl:make-complex-matrix 2 2
                                                        (list (magicl:ref m 2 0) (magicl:ref m 3 0)
                                                              (magicl:ref m 2 1) (magicl:ref m 3 1)))))))
         ;; the division in the above turned one of the entries of b into 1, so
         ;; we can use that to read a off from all the entries of m where that b
         ;; occurs
         (a
           (cond ((or (= state 0) (= state 2))
                  (magicl:make-complex-matrix 2 2
                                              (list (magicl:ref m 0 0) (magicl:ref m 2 0)
                                                    (magicl:ref m 0 2) (magicl:ref m 2 2))))
                 ((or (= state 1) (= state 3))
                  (magicl:make-complex-matrix 2 2
                                              (list (magicl:ref m 1 0) (magicl:ref m 3 0)
                                                    (magicl:ref m 1 2) (magicl:ref m 3 2)))))))
    ;; these are the "right" matrices, but they probably aren't special unitary.
    ;; rescale them to fix this, then hand them back.
    (values
     (magicl:scale (/ (sqrt (magicl:det a))) a)
     (magicl:scale (/ (sqrt (magicl:det b))) b))))

;; these are special matrices that conjugate SU(2) x SU(2) onto SO(4).
;;
;; REM: if/when the above matrices get moved to somewhere central, *these*
;; matrices should not be moved with them. no one cares about them except us.
;; REM: according to /0308033, their only special property is:
;;     e e^T = -(sigma_y (x) sigma_y) .
;; there are several such matrices; this one is just easy to write down.
(alexandria:define-constant
    +e-basis+
    (let* ((sqrt2 (/ (sqrt 2) 2))
           (-sqrt2 (- sqrt2))
           (isqrt2 (complex 0 sqrt2))
           (-isqrt2 (complex 0 -sqrt2)))
      (make-row-major-matrix 4 4
                             (list sqrt2  isqrt2  0       0
                                   0      0       isqrt2  sqrt2
                                   0      0       isqrt2 -sqrt2
                                   sqrt2 -isqrt2  0       0)))
  :test #'matrix-equality
  :documentation "This is an element of SU(4) that has the property e-basis e-basis^T = - sigma_y^((x) 2).")
(alexandria:define-constant
    +edag-basis+
    (let* ((sqrt2 (/ (sqrt 2) 2))
           (-sqrt2 (- sqrt2))
           (isqrt2 (complex 0 sqrt2))
           (-isqrt2 (complex 0 -sqrt2)))
      (make-row-major-matrix 4 4
                             (list sqrt2   0       0      sqrt2
                                  -isqrt2  0       0      isqrt2
                                   0      -isqrt2 -isqrt2 0
                                   0       sqrt2  -sqrt2  0)))
  :test #'matrix-equality
  :documentation "This precomputes the Hermitian transpose of +E-BASIS+.")

;; REM: this is a utility routine that supports diagonalizer-in-e-basis.
;; it seems that when an eigenspace of a complex operator is one-dimensional and
;; it admits a real generator, MAGICL will pick it automatically. (this is
;; because forcing any nonzero entry of the vector to be real forces the rest to
;; be too.) if the eigenspace is pluridimensional, the guarantee goes out the
;; window :( because we expect/require the matrix of eigenvectors to be
;; special-orthogonal, we have to correct this behavior manually.
(defun find-real-spanning-set (vectors)
  "VECTORS is a list of complex vectors in C^n.  When possible, computes a set of vectors with real coefficients that span the same complex subspace of C^n as VECTORS."
  (check-type vectors list)
  (let* ((coeff-matrix (magicl:make-complex-matrix
                        (length (first vectors))
                        (* 2 (length vectors))
                        (concatenate 'list
                                     (loop :for v :in vectors :nconc (map 'list #'imagpart v))
                                     (loop :for v :in vectors :nconc (map 'list #'realpart v)))))
         (reassemble-matrix (magicl:make-complex-matrix
                             (length vectors)
                             (* 2 (length vectors))
                             (concatenate 'list
                                          (loop :for i :from 1 :to (length vectors)
                                                :nconc (loop :for j :from 1 :to (length vectors)
                                                             :collect (if (= i j) 1d0 0d0)))
                                          (loop :for i :from 1 :to (length vectors)
                                                :nconc (loop :for j :from 1 :to (length vectors)
                                                             :collect (if (= i j) #C(0d0 1d0) 0d0))))))
         (backsolved-matrix (reduce #'magicl:multiply-complex-matrices
                                    (list (magicl:make-complex-matrix
                                           (length (first vectors))
                                           (length vectors)
                                           (reduce #'append vectors))
                                          reassemble-matrix
                                          (kernel coeff-matrix))))
         (backsolved-vectors
           (loop :for i :below (magicl:matrix-cols backsolved-matrix)
                 :collect (loop :for j :below (magicl:matrix-rows backsolved-matrix)
                                :collect (magicl:ref backsolved-matrix j i)))))
    (gram-schmidt backsolved-vectors)))

;; this is a support routine for optimal-2q-compile (which explains the funny
;; prefactor multiplication it does).
(defun diagonalizer-in-e-basis (m)
  "For M in SU(4), compute an SO(4) column matrix of eigenvectors of E^* M E (E^* M E)^T."
  (check-type m magicl:matrix)
  (let* ((u (magicl:multiply-complex-matrices +edag-basis+ (magicl:multiply-complex-matrices m +e-basis+)))
         (gammag (magicl:multiply-complex-matrices u (magicl:transpose u))))
    (multiple-value-bind (evals a) (magicl:eig gammag)
      ;; the matrix "a" is almost what we want to return, but it needs to be
      ;; spiced up in various ways:
      ;; + we want its angle values to be sorted descending, so that if we
      ;;   diagonalize two matrices with the same eigenvalues, we automatically
      ;;   get a pair of matrices that conjugate one into the other.
      ;; + we want its 2-dimensional eigenspaces to be spanned by real vectors.
      ;; + we want it to be in SO(4), not O(4).
      ;;
      ;; first, we address the sort issue. the columns of a match the order of
      ;; the the values in angles, so we sort the two lists in parallel.\
      (let* ((angles (map 'list (lambda (x) (let ((ret (imagpart (log x))))
                                         (if (double= ret (- pi)) pi ret)))
                          evals))
             (augmented-list
               (sort
                (loop :for i :below 4
                      :collect (let ((col (loop :for j :below 4
                                                :collect (magicl:ref a j i))))
                                 (list (nth i angles) col)))
                (lambda (x y) (< (first x) (first y)))))
             ;; if vectors lie in the same eigenspace, make them real and orthonormal
             (current-eval (first (first augmented-list)))
             (current-evects (list (second (first augmented-list))))
             (real-data
               (reduce #'append
                       (loop
                         :for pair :in (append (rest augmented-list)
                                               (list (list most-positive-fixnum nil))) ;; this dummy tail item forces a flush
                         :nconc (if (double= current-eval (first pair))
                                    ;; we're still forming the current eigenspace...
                                    (prog1
                                        nil
                                      (setf current-evects (append (list (second pair))
                                                                   current-evects)))
                                    ;; we're ready for a flush and a new eigenspace.
                                    (prog1
                                        (find-real-spanning-set current-evects)
                                      (setf current-eval (first pair))
                                      (setf current-evects (list (second pair)))))))))
        ;; form a matrix out of the results so far.
        (setf a (magicl:make-complex-matrix 4 4 real-data)))
      ;; lastly, fix the determinant (if there's anything left to fix)
      (when (< (realpart (magicl:det a)) 0)
        (setf a (magicl:multiply-complex-matrices a (magicl:diag 4 4 '(-1d0 1d0 1d0 1d0)))))
      a)))

(defun twist-to-real (m)
  "For a matrix M in SU(4), returns a values pair (SIGMA, M') such that M' = M * RZ(sigma) 1 * iSWAP and M' has real chi-gamma polynomial."
  ;; this magical formula was furnished to us by asking Mathematica to compute
  ;; the trace of M' for a symbolic M and SIGMA, then solving
  ;;     0 = imagpart(tr) = imagpart(a cos(sigma) + b sin(sigma)) ,
  ;; where a and b work out to be these disgusting sums below.
  (let* ((sigma (atan (imagpart (+ (*  1d0 (magicl:ref m 1 3) (magicl:ref m 2 0))
                                   (*  1d0 (magicl:ref m 1 2) (magicl:ref m 2 1))
                                   (*  1d0 (magicl:ref m 1 1) (magicl:ref m 2 2))
                                   (*  1d0 (magicl:ref m 1 0) (magicl:ref m 2 3))
                                   (* -1d0 (magicl:ref m 0 3) (magicl:ref m 3 0))
                                   (* -1d0 (magicl:ref m 0 2) (magicl:ref m 3 1))
                                   (* -1d0 (magicl:ref m 0 1) (magicl:ref m 3 2))
                                   (* -1d0 (magicl:ref m 0 0) (magicl:ref m 3 3))))
                      (imagpart (+ (*  1d0 (magicl:ref m 1 2) (magicl:ref m 2 0))
                                   (* -1d0 (magicl:ref m 1 3) (magicl:ref m 2 1))
                                   (*  1d0 (magicl:ref m 1 0) (magicl:ref m 2 2))
                                   (* -1d0 (magicl:ref m 1 1) (magicl:ref m 2 3))
                                   (* -1d0 (magicl:ref m 0 2) (magicl:ref m 3 0))
                                   (*  1d0 (magicl:ref m 0 3) (magicl:ref m 3 1))
                                   (* -1d0 (magicl:ref m 0 0) (magicl:ref m 3 2))
                                   (*  1d0 (magicl:ref m 0 1) (magicl:ref m 3 3)))))))
    (values
     sigma
     (reduce #'magicl:multiply-complex-matrices
             (list
              m
              (su2-on-line 0 (gate-matrix (lookup-standard-gate "RY") sigma))
              (gate-matrix (lookup-standard-gate "ISWAP")))))))

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
                    (:cphase   (string= name "CPHASE"))
                    (otherwise nil))))
           (some #'good requirements)))))

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
  (let* ((ggt (reduce 'magicl:multiply-complex-matrices
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
         instr (alexandria:ensure-list target))
    (give-up-compilation :because ':acts-trivially))

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
                                               :operator (named-operator (format nil "2Q-TWIST-~d" tag))
                                               :arguments (application-arguments instr)
                                               :gate mprime)
                                :target target)))))
                   ;; failure case: no entanglers available
                   (t (return-from optimal-2q-compiler nil)))))
             ;; we also want to know a matrix representation of the bare circuit, so
             ;; that we can use MAGICL to adorn it with SU(2) x SU(2) conjugators.
             (bare-matrix
               (let ((temp (make-gate-matrix-from-gate-string (application-arguments instr)
                                                              bare-circuit)))
                 (unless (= 4 (magicl:matrix-rows temp))
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
             (c (reduce 'magicl:multiply-complex-matrices
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
             (reduce 'magicl:multiply-complex-matrices
                     (list +e-basis+
                           a
                           (magicl:transpose b)
                           +edag-basis+)))
          (multiple-value-bind
                (c1 c0)
              (convert-su4-to-su2x2
               (reduce 'magicl:multiply-complex-matrices
                       (list +e-basis+
                             c
                             +edag-basis+)))
            ;; now we just do the assembly.
            ;; convert the SU(2) matrices to instructions...
            (let ((atb1instrs (euler-compiler (make-instance 'gate-application
                                                             :gate atb1
                                                             :arguments (list (qubit q1))
                                                             :operator (named-operator
                                                                        (format nil "ATB1-~d" tag)))))
                  (atb0instrs (euler-compiler (make-instance 'gate-application
                                                             :gate atb0
                                                             :arguments (list (qubit q0))
                                                             :operator (named-operator
                                                                        (format nil "ATB0-~d" tag)))))
                  (c1instrs (euler-compiler (make-instance 'gate-application
                                                           :gate c1
                                                           :arguments (list (qubit q1))
                                                           :operator (named-operator
                                                                      (format nil "C1-~d" tag)))))
                  (c0instrs (euler-compiler (make-instance 'gate-application
                                                           :gate c0
                                                           :arguments (list (qubit q0))
                                                           :operator (named-operator
                                                                      (format nil "C0-~d" tag))))))
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
