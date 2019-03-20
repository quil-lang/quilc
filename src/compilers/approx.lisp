;;;; approx.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil)


;;; what "shortest gate string" means depends upon what gateset is available.
;;; the TARGET argument is either an OPTIMAL-2Q-TARGET-ATOM or a(n unsorted)
;;; sequence of such atoms, indicating which 2Q gates are available for use.

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


;;; we do some very hands-on linear algebra far below. these next routines are
;;; all in support for that.

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
  :documentation "This is an element of SU(4) that has two properties: (1) e-basis e-basis^T = - sigma_y^((x) 2) and (2) e-basis^dag SU(2)^((x) 2) e-basis = SO(4).")
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
  :documentation "This is a precomputed Hermitian transpose of +E-BASIS+.")

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

(defun orthogonal-decomposition (m)
  (let* ((a (diagonalizer-in-e-basis m))
         (db (reduce #'magicl:multiply-complex-matrices
                     (list (magicl:transpose a) +edag-basis+ m +e-basis+)))
         (diag (loop :for j :below 4
                     :collect (let ((mag 0d0)
                                    phase)
                                (dotimes (i 4)
                                  (when (>= (abs (magicl:ref db j i)) mag)
                                    (setf mag (abs (magicl:ref db j i)))
                                    (setf phase (mod (phase (magicl:ref db j i)) pi))))
                                (exp (* #C(0 1) phase)))))
         (d (magicl:diag 4 4 diag))
         (b (magicl:multiply-complex-matrices
             (magicl:conjugate-transpose d) db)))
    ;; it could be the case that b has negative determinant. if that's the case, we'll
    ;; swap two of its columns that live in the same eigenspace.
    (when (double~ -1d0 (magicl:det b))
      (setf d (magicl:multiply-complex-matrices
               d
               (magicl:diag 4 4 (list -1 1 1 1))))
      (setf b (magicl:multiply-complex-matrices
               (magicl:diag 4 4 (list -1 1 1 1))
               b)))
    (when *compress-carefully*
      (assert (double~ 1d0 (magicl:det m)))
      (assert (double~ 1d0 (magicl:det a)))
      (assert (double~ 1d0 (magicl:det b)))
      (assert (double~ 1d0 (magicl:det d)))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices a (magicl:transpose a))))
      (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                  (magicl:multiply-complex-matrices b (magicl:transpose b))))
      (assert (matrix-equals-dwim (reduce #'magicl:multiply-complex-matrices
                                          (list +edag-basis+ m +e-basis+))
                                  (reduce #'magicl:multiply-complex-matrices
                                          (list a d b)))))
    (values a d b)))

(defun make-signed-permutation-matrix (sigma &optional (signs (list 1 1 1 1)))
  (let ((o (magicl:make-zero-matrix 4 4)))
    (loop :for i :below 4
          :for j :in (cl-permutation:permute sigma (alexandria:iota 4))
          :for sign :in signs
          :do (setf (magicl:ref o i j) sign))
    o))

(defun match-matrix-to-an-e-basis-diagonalization (mprime a d b)
  ;; start by decomposing e^* m' e = a' d' b'.
  ;; we then maximize over signed permutation matrices o so that
  ;;     d' ~~ o^T d o
  ;; is as good an approximation as possible.
  ;; then, naming m = e (a d b) e^*, it follows that
  ;;     m  = e (a d b) e^*
  ;;       ~~ e a o d' o^T b e^*
  ;;        = e a o a'^T a' d' b' b'^T o^T b e^*
  ;;        = (e a o a'^T e^*) (e a' d' b' e^*) (e b'^T o^T b e^*)
  ;;        = (e a o a'^T e^*) m (e b'^T o^T b e^*)
  ;; is as good an approximation as possible.
  ;; we return the SU(2) (x) SU(2) versions of the triple products, as on the
  ;; last line, as well as the fidelity as a values triple.
  (multiple-value-bind (aprime dprime bprime)
      (orthogonal-decomposition (magicl:scale (expt (magicl:det mprime) -1/4) mprime))
    (let (o oT
          (d-as-list      (loop :for j :below 4 :collect (magicl:ref d j j)))
          (dprime-as-list (loop :for j :below 4 :collect (magicl:ref dprime j j)))
          (max-fidelity 0d0))
      ;; maximize the trace over signed permutations
      (cl-permutation:doperms (sigma 4)
        (dolist (signs (list (list  1  1  1  1)
                             (list -1 -1  1  1)
                             (list -1  1 -1  1)
                             (list -1  1  1 -1)))
          (when (= -1 (cl-permutation:perm-sign sigma))
            (setf (first signs) (* -1 (first signs))))
          (let* ((new-trace
                  (loop :for x :in (cl-permutation:permute sigma d-as-list)
                     :for y :in dprime-as-list
                     :for sign :in signs
                     :sum (* x sign (conjugate y))))
                 (new-fidelity (/ (+ 4 (abs (* new-trace new-trace))) 20)))
            (when (> new-fidelity max-fidelity)
              (setf max-fidelity new-fidelity)
              (setf o (make-signed-permutation-matrix sigma))
              (setf oT (magicl:transpose (make-signed-permutation-matrix sigma signs)))))))
      (when *compress-carefully*
        (progn
          (format t "///////////~%~%")
          (format t "A: ~a~%" a)
          (format t "D: ~a~%" d)
          (format t "B: ~a~%" b)
          (format t "EADBE^*: ~a~%" (reduce
                                     #'magicl:multiply-complex-matrices
                                     (list +e-basis+ a d b +edag-basis+)))
          (format t "M': ~a~%" mprime)
          (format t "A': ~a~%" aprime)
          (format t "D': ~a~%" dprime)
          (format t "B': ~a~%" bprime)
          (format t "EA'D'B'E^*: ~a~%" (reduce #'magicl:multiply-complex-matrices
                                               (list +e-basis+ aprime dprime bprime +edag-basis+)))
          (format t "O: ~a~%" o)
          (format t "OT: ~a~%" oT)
          (format t "UA M' UB: ~a~%"
                  (reduce #'magicl:multiply-complex-matrices
                          (list +e-basis+ a o (magicl:transpose aprime) +edag-basis+
                                mprime
                                +e-basis+ (magicl:transpose bprime) oT b +edag-basis+))))
        (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                    (magicl:multiply-complex-matrices a (magicl:transpose a))))
        (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                    (magicl:multiply-complex-matrices b (magicl:transpose b))))
        (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                    (magicl:multiply-complex-matrices aprime (magicl:transpose aprime))))
        (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                    (magicl:multiply-complex-matrices bprime (magicl:transpose bprime))))
        (assert (matrix-equals-dwim (magicl:diag 4 4 '(1d0 1d0 1d0 1d0))
                                    (magicl:multiply-complex-matrices o (magicl:transpose o))))
        (assert (double~ 1d0 (magicl:det a)))
        (assert (double~ 1d0 (magicl:det aprime)))
        (assert (double~ 1d0 (magicl:det b)))
        (assert (double~ 1d0 (magicl:det bprime)))
        (assert (double~ 1d0 (magicl:det o))))
      (values (reduce #'magicl:multiply-complex-matrices
                      (list +e-basis+ a o (magicl:transpose aprime) +edag-basis+))
              (reduce #'magicl:multiply-complex-matrices
                      (list +e-basis+ (magicl:transpose bprime) oT b +edag-basis+))
              max-fidelity))))


;;; we also provide some utilities related specifically to canonical gates and
;;; to trace distance.

(defun trace-distance (m1 m2)
  "Calculates the average fidelity distance between two unitary operators M1 and M2."
  (assert (= (magicl:matrix-rows m1) (magicl:matrix-cols m1)
             (magicl:matrix-rows m2) (magicl:matrix-cols m2)))
  (let* ((n (magicl:matrix-rows m1))
         (prod (magicl:multiply-complex-matrices m1 (magicl:conjugate-transpose m2)))
         (tr (matrix-trace prod)))
    (/ (+ n (abs (* tr tr)))
       (+ n (* n n)))))

(defun fidelity-coord-distance (coord1 coord2)
  (let* ((n 4)
         (delta-a (/ (- (first coord1) (first coord2)) 2))
         (delta-b (/ (- (second coord1) (second coord2)) 2))
         (delta-c (/ (- (third coord1) (third coord2)) 2))
         (tr (+ (* (cos delta-a)
                   (cos delta-b)
                   (cos delta-c))
                (* #C(0 1)
                   (sin delta-a)
                   (sin delta-b)
                   (sin delta-c)))))
    (- 1 (/ (+ n (abs (* 16 tr tr)))
            (+ n (* n n))))))

(defun fidelity-of-straight-quil (instrs chip-spec)
  (let ((ls (make-lscheduler)))
    (append-instructions-to-lschedule ls instrs)
    (lscheduler-calculate-fidelity ls chip-spec)))

(defun get-canonical-coords-from-diagonal (d)
  (assert (= 4 (magicl:matrix-rows d) (magicl:matrix-cols d)))
  (let* ((angles (mapcar #'phase (loop :for i :below 4 :collect (magicl:ref d i i))))
         (first  (mod    (+ (third angles) (fourth angles)) pi))
         (second (mod (- (+ (third angles) (first angles))) pi))
         (third  (mod    (+ (third angles) (second angles)) pi))
         (first  (if (double= pi first)  0d0 first))
         (second (if (double= pi second) 0d0 second))
         (third  (if (double= pi third)  0d0 third))
         (option-1 (sort (list first second third) #'>)))
    (destructuring-bind (first second third) option-1
      (let ((option-2 (sort (list (- pi first)       (- pi second) third)        #'>))
            (option-3 (sort (list first              (- pi second) (- pi third)) #'>))
            (option-4 (sort (list (- pi first)       second        (- pi third)) #'>))
            (option-5 (sort (list (- first pi)       second        third)        #'>))
            (option-6 (sort (list (- first pi)       (- pi second) (- pi third)) #'>))
            (option-7 (sort (list (- (* 2 pi) first) (- pi second) third)        #'>))
            (option-8 (sort (list (- (* 2 pi) first) second        (- pi third)) #'>)))
        (flet ((test (seq) (double>= (/ pi 2) (first seq) (second seq) (abs (third seq)))))
          (cond
            ((test option-1) option-1)
            ((test option-2) option-2)
            ((test option-3) option-3)
            ((test option-4) option-4)
            ((test option-5) option-5)
            ((test option-6) option-6)
            ((test option-7) option-7)
            ((test option-8) option-8)
            (t (error "uh-oh (cf. ~a)" (list first second third)))))))))

(defun build-canonical-gate (coord)
  (destructuring-bind (c1 c2 c3) coord
    (reduce #'magicl:multiply-complex-matrices
            (list +e-basis+
                  (magicl:diag 4 4
                               (mapcar (lambda (z) (exp (* #C(0 0.5d0) z)))
                                       (list (+ c1 (- c2) c3)
                                             (+ c1 c2 (- c3))
                                             (- (+ c1 c2 c3))
                                             (+ (- c1) c2 c3))))
                  +edag-basis+))))


;;; now, finally, we start writing approximation-specific things.  what follows
;;; are templates that plug into the general framework of approximate compilation.
;;; these are accreted into the global **approximate-template-records**, which
;;; will need flushing if you ever need to modify a particular template.
;;; otherwise, this list of records will retain both the record pointing to the
;;; new definition as well as the previously existing record pointing to the old.
;;;
;;; important note: these are listed in *ascending* preference order.

(defvar **approximate-template-records** nil
  "Houses a list of available approximate templates.")

(setf **approximate-template-records** nil)

(defstruct approximate-template-record
  function
  predicate
  requirements)

(defmacro define-approximate-template (name (&rest args) requirements pred &body body)
  `(progn
     (defun ,name (,@args)
       (unless (or *enable-approximate-compilation*
                   ,pred)
         (give-up-compilation))
       ,@body)
     (push (make-approximate-template-record
            :function #',name
            :predicate (lambda (,@args)
                         (declare (ignorable ,@args))
                         ,pred)
            :requirements ,requirements)
           **approximate-template-records**)))

(defmacro define-searching-approximate-template (name (&rest args) reqs pred parameter-count &body parametric-circuit)
  "Defines an approximate template that uses a Nelder-Mead solver."
  `(define-approximate-template ,name (,(first args) ,(second args) ,(third args))
       ,reqs
       ,pred
     (flet ((circuit-template (,(fourth args) q1 q0)
              ,@parametric-circuit))
       (multiple-value-bind (template-values goodness)
           (cl-grnm:nm-optimize
            (lambda (in)
              (multiple-value-bind (a d b)
                  (orthogonal-decomposition (make-matrix-from-quil (circuit-template in 1 0)))
                (declare (ignore a b))
                (fidelity-coord-distance ,(first args) (get-canonical-coords-from-diagonal d))))
            (make-array ,parameter-count :initial-element 1d0))
         (unless (or *enable-approximate-compilation*
                     (double= 0d0 goodness))
           (give-up-compilation))
         (values (circuit-template template-values ,(second args) ,(third args)) goodness)))))


(define-approximate-template nearest-CZ-circuit-of-depth-3 (coord q1 q0)
    '(:cz)
    t
  (let ((alpha (- (first coord) pi))
        (beta  (- pi            (second coord)))
        (gamma (- (/ pi 2)      (third coord))))
    (list (build-gate "CZ" '()            q0 q1)
          (build-gate "RY" '(#.(/ pi -2)) q0)
          (build-gate "RY" (list alpha)   q1)
          (build-gate "RZ" (list gamma)   q0)
          (build-gate "CZ" '()            q0 q1)
          (build-gate "RY" (list alpha)   q1)
          (build-gate "RY" '(#.(/ pi 2))  q0)
          (build-gate "CZ" '()            q0 q1))))

(define-approximate-template nearest-CZ-circuit-of-depth-2 (coord q1 q0)
    '(:cz)
    (double= 0d0 (third coord))
  (list (build-gate "CZ" () q1 q0)
        (build-gate "RY" (list (first coord)) q1)
        (build-gate "RY" (list (second coord)) q0)
        (build-gate "CZ" () q1 q0)))

(define-approximate-template nearest-ISWAP-circuit-of-depth-3 (coord q1 q0)
    '(:iswap)
    t
  (flet ((twist-to-real (m)
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
                       (gate-matrix (lookup-standard-gate "ISWAP"))))))))
    ;; TODO: is TWIST-TO-REAL easier to compute when coupled to BUILD-CANONICAL-GATE?
    (multiple-value-bind (sigma mprime) (twist-to-real (build-canonical-gate coord))
      (multiple-value-bind (a d b) (orthogonal-decomposition mprime)
        (let ((coordprime (get-canonical-coords-from-diagonal d)))
          (list* (build-gate "RY"    (list (- sigma)) q0)
                 (build-gate "ISWAP" '()              q1 q0)
                 (build-gate "Z"     '()              q0)
                 (build-gate "Z"     '()              q1)
                 (sandwich-with-local-gates
                  (nearest-ISWAP-circuit-of-depth-2 coordprime q1 q0)
                  a d b q1 q0)))))))

(define-searching-approximate-template nearest-XY-XY-template-of-depth-2 (coord q1 q0 array)
    '(:cphase :iswap)
    t                       ; TODO: replace this with a convexity test
    6
  (let ((first  (aref array 0))
        (second (aref array 1))
        (third  (aref array 2))
        (fourth (aref array 3))
        (fifth  (aref array 4))
        (sixth  (aref array 5)))
    (list
     (build-gate "PISWAP" (list third)     q0 q1)
     (build-gate "RZ"     (list fifth)     q0)
     (build-gate "RZ"     (list (- fifth)) q1)
     (build-gate "RY"     (list first)     q0)
     (build-gate "RY"     (list second)    q1)
     (build-gate "RZ"     (list sixth)     q0)
     (build-gate "RZ"     (list (- sixth)) q1)
     (build-gate "PISWAP" (list fourth)    q0 q1))))

(define-searching-approximate-template nearest-CPHASE-XY-template-of-depth-2 (coord q1 q0 array)
  '(:cphase :piswap)
  t                         ; TODO: replace this with a convexity test
  5
  (list
   (build-gate "CPHASE" (list (aref array 4)) q1 q0)
   (build-gate "RY" (list (aref array 0)) q0)
   (build-gate "RY" (list (aref array 1)) q1)
   (build-gate "RZ" (list (aref array 2)) q0)
   (build-gate "RZ" (list (- (aref array 2))) q1)
   (build-gate "PISWAP" (list (aref array 3)) q1 q0)))

(define-searching-approximate-template nearest-CZ-XY-template-of-depth-2 (coord q1 q0 array)
  '(:cz :piswap)
  t                         ; TODO: replace this with a convexity test
  4
  (list
   (build-gate "CZ"     ()                     q1 q0)
   (build-gate "RY"     (list (aref array 0))     q0)
   (build-gate "RY"     (list (aref array 1))     q1)
   (build-gate "RZ"     (list (aref array 2))     q0)
   (build-gate "RZ"     (list (- (aref array 2))) q1)
   (build-gate "PISWAP" (list (aref array 3)) q1 q0)))

(define-searching-approximate-template nearest-CPHASE-ISWAP-template-of-depth-2 (coord q1 q0 array)
    '(:cphase :iswap)
    t                       ; TODO: replace this with a convexity test
    3
  (list
   (build-gate "ISWAP"  ()                    q0 q1)
   (build-gate "RY"     (list (aref array 0)) q0)
   (build-gate "RY"     (list (aref array 1)) q1)
   (build-gate "CPHASE" (list (aref array 2)) q0 q1)))

(define-approximate-template nearest-CZ-ISWAP-circuit-of-depth-2 (coord q1 q0)
    '(:cz :iswap)
    (double= (/ pi 2) (first coord))
  (list (build-gate "ISWAP" () q1 q0)
        (build-gate "RY" (list (- (/ pi 2) (second coord))) q0)
        (build-gate "RY" (list (- (/ pi 2) (third coord))) q1)
        (build-gate "CZ" () q1 q0)))

(define-approximate-template nearest-ISWAP-circuit-of-depth-2 (coord q1 q0)
    '(:iswap)
    (double= 0d0 (third coord))
  (list (build-gate "ISWAP" '()          q1 q0)
        (build-gate "RY"    (list (first coord)) q1)
        (build-gate "RY"    (list (second coord)) q0)
        (build-gate "ISWAP" '()          q1 q0)))

(define-approximate-template nearest-CPHASE-circuit-of-depth-1 (coord q1 q0)
    '(:cphase)
    (every #'double= (rest coord) (list 0d0 0d0))
  (list (build-gate "CPHASE" (list (first coord)) q1 q0)))

(define-approximate-template nearest-CZ-circuit-of-depth-1 (coord q1 q0)
    '(:cz)
    (every #'double= coord (list (/ pi 2) 0d0 0d0))
  (list (build-gate "CZ" () q1 q0)))

(define-approximate-template nearest-XY-circuit-of-depth-1 (coord q1 q0)
    '(:piswap)
    (and (double= (first coord) (second coord))
         (double= (third coord) 0d0))
  (list (build-gate "PISWAP" (list (first coord)) q1 q0)))

(define-approximate-template nearest-ISWAP-circuit-of-depth-1 (coord q1 q0)
    '(:iswap)
    (every #'double= coord (list (/ pi 2) (/ pi 2) 0))
  (list (build-gate "ISWAP" '() q1 q0)))

(define-approximate-template nearest-circuit-of-depth-0 (coord q1 q0)
    '()
    (every #'double= coord (list 0d0 0d0 0d0))
  (list (build-gate "I" () q0)
        (build-gate "I" () q1)))


;;; here lies the logic underlying the approximate compilation routine.

(defun sandwich-with-local-gates (center-circuit a d b q1 q0)
  "Given a circuit CENTER-CIRCUIT and an E-basis-diagonalized operator (A, D, B) (as returned by ORTHOGONAL-DECOMPOSITION), this routine computes an extension of CENTER-CIRCUIT by local gates which maximizes the trace fidelity with the product (E-BASIS)ADB(EDAG-BASIS)."
  (format t "SANDWICH-... called with circuit ~a~%~%" center-circuit)
  (multiple-value-bind (ua ub fidelity)
      (match-matrix-to-an-e-basis-diagonalization
       (make-matrix-from-quil center-circuit)
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

(defun approximate-2Q-compiler (instr &key (chip-spec nil) (crafters nil))
  "Generic logic for performing (approximate) two-qubit compilation.  This consumes an instruction INSTR to compile, an object CHIP-SPEC of type CHIP-SPECIFICATION which records fidelity information, and a list of circuit template manufacturers CRAFTERS to run through."
  (check-type instr gate-application)
  (check-type chip-spec chip-specification)
  
  (unless (= 2 (length (application-arguments instr)))
    (give-up-compilation))
  
  ;; extract matrix, canonical decomposition
  (let* ((q1 (qubit-index (first (application-arguments instr))))
         (q0 (qubit-index (second (application-arguments instr))))
         (m (gate-matrix instr))
         (m (magicl:scale (expt (magicl:det m) -1/4) m)))
    (multiple-value-bind (a d b) (orthogonal-decomposition m)
      ;; now we manufacture a bunch of candidate circuits
      (let* ((candidate-pairs nil)
             (coord (get-canonical-coords-from-diagonal d)))
        (dolist (circuit-crafter crafters)
          (handler-case
              (let* ((center-circuit (apply circuit-crafter coord (mapcar #'qubit-index
                                                                          (application-arguments instr))))
                     (ls (append-instructions-to-lschedule (make-lscheduler) center-circuit))
                     (circuit-cost (lscheduler-calculate-fidelity ls chip-spec)))
                (multiple-value-bind (sandwiched-circuit fidelity)
                    (sandwich-with-local-gates center-circuit a d b q1 q0)
                  (format *compiler-noise-stream*
                          "APPROXIMATE-2Q-COMPILER: ~a resulted in~%    ... lone fidelity ~a~%    ... and cost ~a,~%    ... hence adjusted fidelity ~a.~%"
                          circuit-crafter fidelity circuit-cost (* circuit-cost fidelity))
                  (push (cons (* circuit-cost fidelity) sandwiched-circuit)
                        candidate-pairs)))
            (compiler-does-not-apply () nil)))
        ;; now vomit the results
        (cond
          ((endp candidate-pairs)
           (give-up-compilation))
          (t
           (cdr (alexandria:extremum candidate-pairs #'> :key #'car))))))))

(defun approximate-2Q-compiler-for (target chip-spec)
  "Constructs an approximate 2Q compiler suitable for a TARGET architecture and a CHIP-SPEC with fidelity data.  Returns a function to be installed into the compilers present on CHIP-SPEC."
  (let ((crafters
         (mapcar #'approximate-template-record-function
                 (remove-if-not (lambda (record)
                                  (optimal-2q-target-meets-requirements
                                   target
                                   (approximate-template-record-requirements record)))
                                **approximate-template-records**))))
    (lambda (instr)
      (approximate-2Q-compiler instr
                               :chip-spec chip-spec
                               :crafters crafters))))
