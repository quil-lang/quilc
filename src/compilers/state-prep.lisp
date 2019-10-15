;;;; state-prep.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; The trampolining decomposition algorithm is based off of Section 4 of
;;;; arXiv:0406176, our old QSC favorite.

(in-package #:cl-quil)

(defclass state-prep-application (gate-application)
  ((source-wf :initarg :source-wf
              :accessor state-prep-application-source-wf
              :documentation "Source wavefunction.")
   (target-wf :initarg :target-wf
              :accessor state-prep-application-target-wf
              :documentation "Target wavefunction."))
  (:default-initargs :operator #.(named-operator "STATE-PREP")
                     ;; XXX: Hack!
                     :gate nil)
  (:documentation "A pseudo-instruction representing any state-preparation circuit that carries SOURCE-WF into TARGET-WF."))

;;; XXX: Hack!
(defmethod gate-matrix ((gate state-prep-application) &rest parameters)
  (declare (ignore gate parameters))
  nil)

(defmethod print-instruction-generic ((thing state-prep-application) (s stream))
  (format s "STATE-PREP[(~{~5$~^, ~}) -> (~{~5$~^, ~})] ~{~/quil:instruction-fmt/~^ ~}"
          (coerce (state-prep-application-source-wf thing) 'list)
          (coerce (state-prep-application-target-wf thing) 'list)
          (application-arguments thing)))


;; first we do base case work.
;; the most basic base case is the case of a 1-qubit operator.
(define-compiler state-prep-1Q-compiler
    ((instr (_ _ q)
            :where (typep instr 'state-prep-application)))
  "Compiler for STATE-PREP-APPLICATION instances that target a single qubit."
  (let* ((source-wf (vector-scale
                     (/ (norm (coerce (state-prep-application-source-wf instr) 'list)))
                     (coerce (state-prep-application-source-wf instr) 'list)))
         (target-wf (vector-scale
                     (/ (norm (coerce (state-prep-application-target-wf instr) 'list)))
                     (coerce (state-prep-application-target-wf instr) 'list)))
         (matrix-target (magicl:make-complex-matrix
                         2 2
                         (append target-wf
                                 (list (- (conjugate (second target-wf)))
                                       (conjugate (first target-wf))))))
         (matrix-source (magicl:make-complex-matrix
                         2 2
                         (list (conjugate (first source-wf))
                               (- (second source-wf))
                               (conjugate (second source-wf))
                               (first source-wf)))))
    (inst "STATE-1Q" (m* matrix-target matrix-source) q)))


;; setting up 2Q state preparation requires some helper functions
(defun split-product-state-vector (vector) ; #(ac bc ad bd)
  "Takes a vector of the form #(A*C B*C A*D B*D) and returns a pair of vectors (#(A B) #(C D))."
  (cond
    ;; ac = 0 and bc = 0 implies c = 0
    ((and (double= 0d0 (aref vector 0))
          (double= 0d0 (aref vector 1)))
     (list (make-array 2 :initial-contents (list (aref vector 2)
                                                 (aref vector 3))
                         :element-type '(complex double-float))
           (make-array 2 :initial-contents (list #C(0d0 0d0) #C(1d0 0d0))
                         :element-type '(complex double-float))))
    ;; ac = 0 and bc != 0 implies a = 0, c != 0
    ((double= 0d0 (aref vector 0))
     (list (make-array 2 :initial-contents (list #C(0d0 0d0) #C(1d0 0d0))
                         :element-type '(complex double-float))
           (make-array 2 :initial-contents (list (aref vector 1)
                                                 (aref vector 3))
                         :element-type '(complex double-float))))
    ;; ac != 0 and bc != 0 implies a != 0, c != 0
    (t
     (let* ((n1 (norm (list (aref vector 0)
                            (aref vector 2)))))
       (list (make-array 2 :initial-contents (list (complex n1 0d0)
                                                   (* (aref vector 1)
                                                      n1
                                                      (/ (aref vector 0))))
                           :element-type '(complex double-float))
             (make-array 2 :initial-contents (list (/ (aref vector 0) n1)
                                                   (/ (aref vector 2) n1))
                           :element-type '(complex double-float)))))))

(defun segre-obstruction-2Q (vector)
  "Calculates an obstruction, presented as a real number, to factoring [a : b : c : d] as a product state. This number vanishes if and only if such a factorization is possible."
  (- (* (aref vector 0) (aref vector 3))
     (* (aref vector 1) (aref vector 2))))

(defun canonicalize-wf (vector)
  "Calculates a special-orthogonal MATRIX that moves a unit-length VECTOR in C^4 into the form V = (a+bi c 0 0).  Returns (VALUES MATRIX V)."
  (let ((matrix (magicl:diag 4 4 (list 1d0 1d0 1d0 1d0)))
        (v (copy-seq vector)))
    ;; start by moving all of the imaginary components into the 0th position.
    (unless (double= 0d0 (imagpart (aref v 1)))
      (let* ((theta (- (atan (imagpart (aref v 1))
                             (imagpart (aref v 0)))))
             (m (magicl:make-complex-matrix 4 4 (list (cos theta) (sin theta) 0 0
                                                      (- (sin theta)) (cos theta) 0 0
                                                      0 0 1 0
                                                      0 0 0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    (unless (double= 0d0 (imagpart (aref v 2)))
      (let* ((theta (- (atan (imagpart (aref v 2))
                             (imagpart (aref v 0)))))
             (m (magicl:make-complex-matrix 4 4 (list (cos theta) 0 (sin theta) 0
                                                      0 1 0 0
                                                      (- (sin theta)) 0 (cos theta) 0
                                                      0 0 0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    (unless (double= 0d0 (imagpart (aref v 3)))
      (let* ((theta (- (atan (imagpart (aref v 3))
                             (imagpart (aref v 0)))))
             (m (magicl:make-complex-matrix 4 4 (list (cos theta) 0 0 (sin theta)
                                                      0 1 0 0
                                                      0 0 1 0
                                                      (- (sin theta)) 0 0 (cos theta)))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    ;; now move the real components into the 1st position (except for 0)
    (unless (double= 0d0 (realpart (aref v 2)))
      (let* ((theta (- (atan (realpart (aref v 2))
                             (realpart (aref v 1)))))
             (m (magicl:make-complex-matrix 4 4 (list 1 0 0 0
                                                      0 (cos theta) (sin theta) 0
                                                      0 (- (sin theta)) (cos theta) 0
                                                      0 0 0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    (unless (double= 0d0 (realpart (aref v 3)))
      (let* ((theta (- (atan (realpart (aref v 3))
                             (realpart (aref v 1)))))
             (m (magicl:make-complex-matrix 4 4 (list 1 0 0 0
                                                      0 (cos theta) 0 (sin theta)
                                                      0 0 1 0
                                                      0 (- (sin theta)) 0 (cos theta)))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    ;; we're concentrated in positions 0 and 1.
    ;; if 0 and 1 are actually both real, combine them.
    (unless (or (double= 0d0 (realpart (aref v 1)))
                (not (double= 0d0 (imagpart (aref v 0)))))
      (let* ((theta (- (atan (realpart (aref v 1))
                             (realpart (aref v 0)))))
             (m (magicl:make-complex-matrix 4 4 (list (cos theta) (sin theta) 0 0
                                                      (- (sin theta)) (cos theta) 0 0
                                                      0 0 1 0
                                                      0 0 0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    ;; make sure |v0| > |v1|
    (unless (> (abs (aref v 0)) (abs (aref v 1)))
      (let* ((m (magicl:make-complex-matrix 4 4 (list 0 1 0 0
                                                      -1 0 0 0
                                                      0 0 1 0
                                                      0 0 0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    ;; we're imagining v1 to be purely real (but it might be purely imaginary).
    ;; constrain -pi/4 < phase v1 <= 3pi/4, which contains 0 and pi/2
    (unless (<= (- (/ pi 4)) (phase (aref v 1)) (* 3/4 pi))
      (let* ((m (magicl:make-complex-matrix 4 4 (list 1  0  0 0
                                                      0 -1  0 0
                                                      0  0 -1 0
                                                      0  0  0 1))))
        (setf matrix (m* m matrix))
        (setf v (nondestructively-apply-matrix-to-vector matrix vector))))
    ;; also constrain -pi/2 < phase v0 - phase v1 <= pi/2
    (let* ((phase-difference (- (phase (aref v 0)) (phase (aref v 1))))
           (centered-phase (- (mod (+ phase-difference pi) 2pi) pi)))
      (when (and (not (double= centered-phase pi/2))
                 (or (<= centered-phase -pi/2)
                     (double= centered-phase -pi/2)
                     (< pi/2 centered-phase)))
        (let* ((m (magicl:make-complex-matrix 4 4 (list -1 0  0 0
                                                        0 1  0 0
                                                        0 0 -1 0
                                                        0 0  0 1))))
          (setf matrix (m* m matrix))
          (setf v (nondestructively-apply-matrix-to-vector matrix vector)))))
    (values matrix v)))

(adt:defdata tensor-factorization
  (tensor-pair magicl:matrix magicl:matrix))

(defun state-prep-within-local-equivalence-class (source-wf target-wf)
  "Produces a circuit that carries a two-qubit wavefunction SOURCE-WF to a two-qubit target wavefunction TARGET-WF under the hypothesis that SOURCE-WF and TARGET-WF have the same Segre obstruction.

Returns a pair (LIST C0 C1) of 2x2 matrices with (C0 (x) C1) SOURCE-WF = TARGET-WF."
  (let* ((phase-so-source (phase (segre-obstruction-2Q source-wf)))
         (phase-so-target (phase (segre-obstruction-2Q target-wf)))
         (multiplier (cis (/ (- phase-so-target phase-so-source) 2))))
    ;; rotate the source into the target
    (setf source-wf (map 'vector (a:curry '* multiplier) source-wf))
    ;; find s, t in SO(4) that puts the source and target wfs expressed in the
    ;; magic basis into a canonical form
    (let ((source-matrix (canonicalize-wf (nondestructively-apply-matrix-to-vector +edag-basis+ source-wf)))
          (target-matrix (canonicalize-wf (nondestructively-apply-matrix-to-vector +edag-basis+ target-wf))))
      ;; write t^dag s as a member of SU(2) x SU(2)
      (multiple-value-bind (c1 c0)
          (convert-su4-to-su2x2
           (m* +e-basis+
               (magicl:conjugate-transpose target-matrix)
               source-matrix
               +edag-basis+))
        (tensor-pair c0 c1))))) ;; first arg, second arg

;; this method is due to Oscar Perdomo, and this implementation blindly follows his notes.
;; TODO: this should be made architecture-sensitive, with separate templates
;;       for ISWAP-based chips
(define-compiler state-prep-2Q-compiler
    ((instr (_ _ q1 q0)
            :where (typep instr 'state-prep-application)))
  "Exact, optimal compiler for STATE-PREP-APPLICATION instances that target a pair of qubits."
  (flet ((orthogonal-vector (v)
           (make-row-major-matrix 2 1 (list (- (conjugate (magicl:ref v 1 0)))
                                            (conjugate (magicl:ref v 0 0)))))
         (normalize-vector (v)
           (let ((norm (sqrt (+ (expt (abs (magicl:ref v 0 0)) 2)
                                (expt (abs (magicl:ref v 1 0)) 2)))))
             (if (double= 0d0 norm) v (magicl:scale (/ norm) v))))
         ;; this dodges a numerical stability bullet: the bad taylor expansion of
         ;; arcsin at 1 makes input error on the order of e-17 blow up to output
         ;; error on the order of e-8, which exceeds +double-comparison-threshold-strict+.
         ;; more exactly, the output error gets too wild when the input error
         ;; exceeds e-13 --- but this is below the usual measure of +d-c-t-s+, so
         ;; we clamp the value and pray.
         (asin-nice (x)
           (if (double= x 1d0)
               pi/2
               (asin x))))
    (let* ((A (make-row-major-matrix 2 2 (coerce (state-prep-application-source-wf instr) 'list)))
           (B (make-row-major-matrix 2 2 (coerce (state-prep-application-target-wf instr) 'list)))
           (mA (m* (magicl:conjugate-transpose A) A))
           (mB (m* (magicl:conjugate-transpose B) B)))
      ;; this routine works requires the source to be entangled.
      (cond
        ;; if the source is unentangled and the target is entangled...
        ((and (double= 0d0 (magicl:det mA))
              (not (double= 0d0 (magicl:det mB))))
         ;; rewrite unentangled wavefunction as |00>
         (adt:with-data (tensor-pair cL cR)
                        (state-prep-within-local-equivalence-class
                         (state-prep-application-source-wf instr)
                         (make-array 4 :element-type '(complex double-float)
                                       :initial-contents (list #C(1d0 0d0) #C(0d0 0d0) #C(0d0 0d0) #C(0d0 0d0))))
           (inst "cL" cL q1)
           (inst "cR" cR q0))
         ;; calculate the segre obstruction of the target wf
         (let* ((so (abs (segre-obstruction-2Q (state-prep-application-target-wf instr))))
                (so-angle (realpart (asin-nice (* 2 so))))
                ;; NOTE: this is "duplicated" from the RY/CNOT pair below. it would
                ;;       be better if we could programmatically calculate this!
                (partial-wf (make-array 4 :element-type '(complex double-float)
                                          :initial-contents (list (complex (cos (/ so-angle 2)))
                                                                  #C(0d0 0d0)
                                                                  #C(0d0 0d0)
                                                                  (complex (sin (/ so-angle 2)))))))
           ;; insert an RY and a CNOT which carries |00> to the same segre obstruction
           (inst "RY" (list so-angle) q1)
           (inst "CNOT" () q1 q0)
           ;; calculate local gates witnessing the local equivalence
           (adt:with-data (tensor-pair aL aR)
                          (state-prep-within-local-equivalence-class
                           partial-wf
                           (state-prep-application-target-wf instr))
             (inst "aL" aL q1)
             (inst "aR" aR q0))))
        ;; if the target is unentangled and the source is entangled, we flip
        ;; their order and fall back on the previous branch.
        ((and (not (double= 0d0 (magicl:det mA)))
              (double= 0d0 (magicl:det mB)))
         (dolist (instr (reverse
                         (state-prep-2q-compiler
                          (make-instance 'state-prep-application
                                         :source-wf (state-prep-application-target-wf instr)
                                         :target-wf (state-prep-application-source-wf instr)
                                         :operator (named-operator "REV-STATE-PREP")
                                         :arguments (application-arguments instr)))))
           (adt:with-data (named-operator name) (application-operator instr)
             (cond
               ((string= "CNOT" name)
                (inst instr))
               ((string= "RY" name)
                (inst* "RY"
                       (list (- (constant-value (first (application-parameters instr)))))
                       (application-arguments instr)))
               (t
                (inst* name
                       (magicl:conjugate-transpose (gate-matrix (gate-application-gate instr)))
                       (application-arguments instr)))))))
        ;; if they're both unentangled, we calculate the factorization and
        ;; compile them separately.
        ((and (double= 0d0 (magicl:det mA))
              (double= 0d0 (magicl:det mB)))
         (destructuring-bind (source-0 source-1) (split-product-state-vector
                                                  (state-prep-application-source-wf instr))
           (destructuring-bind (target-0 target-1) (split-product-state-vector
                                                    (state-prep-application-target-wf instr))
             (inst (make-instance 'state-prep-application
                                  :target-wf target-0
                                  :source-wf source-0
                                  :arguments (list (qubit q1))
                                  :operator (named-operator "LEFT-STATE-PREP-FACTOR")))
             (inst (make-instance 'state-prep-application
                                  :target-wf target-1
                                  :source-wf source-1
                                  :arguments (list (qubit q0))
                                  :operator (named-operator "RIGHT-STATE-PREP-FACTOR"))))))
        ;; otherwise, we can proceed as perdomo prescribes.
        (t
         (multiple-value-bind (lambdas a-vectors) (magicl:eig mA)
           (multiple-value-bind (deltas x-vectors) (magicl:eig mB)
             (let* ((a-vector (make-row-major-matrix 2 1 (list (magicl:ref a-vectors 0 1)
                                                               (magicl:ref a-vectors 1 1))))
                    (x-vector (make-row-major-matrix 2 1 (list (magicl:ref x-vectors 0 1)
                                                               (magicl:ref x-vectors 1 1))))
                    (b-vector (orthogonal-vector a-vector))
                    (y-vector (orthogonal-vector x-vector))
                    (c (normalize-vector (m* A a-vector)))
                    (d (normalize-vector (m* A b-vector)))
                    (z (normalize-vector (m* B x-vector)))
                    (u (normalize-vector (m* B y-vector)))
                    (theta1 (- pi/2 (phase (+ (sqrt (first lambdas)) (* #C(0 1) (sqrt (second lambdas)))))))
                    (theta2 (- pi/2 (phase (+ (sqrt (first deltas)) (* #C(0 1) (sqrt (second deltas)))))))
                    (U1 (make-row-major-matrix 2 2 (list (magicl:ref c 0 0) (magicl:ref d 0 0)
                                                         (magicl:ref c 1 0) (magicl:ref d 1 0))))
                    (U2 (make-row-major-matrix 2 2 (list (magicl:ref z 0 0) (magicl:ref u 0 0)
                                                         (magicl:ref z 1 0) (magicl:ref u 1 0))))
                    (V1 (make-row-major-matrix 2 2 (mapcar #'conjugate
                                                           (list (magicl:ref a-vector 0 0) (magicl:ref b-vector 0 0)
                                                                 (magicl:ref a-vector 1 0) (magicl:ref b-vector 1 0)))))
                    (V2 (make-row-major-matrix 2 2 (mapcar #'conjugate
                                                           (list (magicl:ref x-vector 0 0) (magicl:ref y-vector 0 0)
                                                                 (magicl:ref x-vector 1 0) (magicl:ref y-vector 1 0))))))
               (let ((L1 (m* U1
                             (gate-matrix (build-gate "RY" (list (*  2 theta2)) 0))
                             (magicl:conjugate-transpose U1)))
                     (L2 (m* U1
                             (gate-matrix (build-gate "RY" (list (* -2 theta1)) 0))
                             (magicl:conjugate-transpose U1))))
                 ;; U1' L1 (x) V1'
                 (inst "V1-DAG" (magicl:conjugate-transpose V1) q1)
                 (inst "L1"     L1 q0)
                 (inst "U1-DAG" (magicl:conjugate-transpose U1) q0)
                 
                 (inst "CNOT"   () q0 q1)
                 
                 ;; L2 U1 (x) V1
                 (inst "V1"     V1 q1)
                 (inst "U1"     U1 q0)
                 (inst "L2"     L2 q0)
                 
                 ;; L3 = U2 U1' (x) V2 V1'
                 (inst "V1-DAG" (magicl:conjugate-transpose V1) q1) ; begin L3
                 (inst "V2"     V2 q1)
                 (inst "U1-DAG" (magicl:conjugate-transpose U1) q0)
                 (inst "U2"     U2 q0))))))))))

;; here's a previous version of the 2Q compiler. it uses nelder-mead, so doesn't produce as
;; pretty of results, but it does rely on this interesting bit of geometry where it
;; "canonicalizes" the wavefunction through rotation into something of a very particular
;; form. there's probably a meet between the geometry in this routine and the mysterious
;; formulas in Oscar's. it would be nice to write out exactly what this meet is.
;;
;; some of the code in this has been pulled down to state-prep-within-local-equivalence-class
;; below. if anyone wants to turn this routine back on, it's worth offloading some
;; of the code in here to that subroutine.
#+#:pedagogical-purposes-only
(define-compiler state-prep-2Q-compiler
    ((instr (_ _ _ _)
            :where (typep instr 'state-prep-application))) 
  "Compiler for STATE-PREP-APPLICATION instances that target a pair of qubits."
  (let ((qubit-complex (reverse (mapcar #'qubit-index (application-arguments instr))))
        prefix-circuit
        (source-wf (state-prep-application-source-wf instr))
        (target-wf (state-prep-application-target-wf instr)))
    (let ((abs-so-source (abs (segre-obstruction-2Q source-wf)))
          (abs-so-target (abs (segre-obstruction-2Q target-wf))))
      (unless (double= abs-so-source abs-so-target)
        ;; move the source wf's SO to the target wf's using N-M
        (labels ((build-circuit (phi0 theta0 phi1 theta1)
                   (destructuring-bind (p q) (application-arguments instr)
                     (list
                      (build-gate "RZ" `(,phi1)   q)
                      (build-gate "RY" `(,theta1) q)
                      (build-gate "RZ" `(,phi0)   p)
                      (build-gate "RY" `(,theta0) p)
                      (build-gate "CZ"  ()        q p))))
                 (objective-fn (vector)
                   (destructuring-bind (phi0 theta0 phi1 theta1)
                       (coerce vector 'list)
                     (let* ((instrs (build-circuit phi0 theta0 phi1 theta1))
                            (out-vector (nondestructively-apply-instrs-to-wf
                                         instrs
                                         source-wf
                                         qubit-complex)))
                       (abs (- abs-so-target
                               (abs (segre-obstruction-2Q out-vector))))))))
          (multiple-value-bind (result goodness)
              (cl-grnm:nm-optimize #'objective-fn #(0d0 0d0 0d0 0d0))
            (unless (double= 0d0 goodness)
              (return-from state-prep-2Q-compiler
                (state-prep-trampolining-compiler instr)))
            (destructuring-bind (phi0 theta0 phi1 theta1)
                (coerce result 'list)
              (setf prefix-circuit (build-circuit phi0 theta0 phi1 theta1))
              (setf source-wf (nondestructively-apply-instrs-to-wf prefix-circuit
                                                                   source-wf
                                                                   qubit-complex))
              (setf abs-so-source (abs (segre-obstruction-2Q source-wf)))
              (dolist (instr prefix-circuit)
                (inst instr)))))))
    (let ((phase-so-source (phase (segre-obstruction-2Q source-wf)))
          (phase-so-target (phase (segre-obstruction-2Q target-wf))))
      (unless (double= phase-so-source phase-so-target)
        ;; rotate the source into the target
        (let ((multiplier (cis (/ (- phase-so-target phase-so-source) 2))))
          (dotimes (j 4)
            (setf (aref source-wf j)
                  (* multiplier
                     (aref source-wf j)))))))
    ;; find s, t in SO(4) that puts the source and target wfs expressed in the magic basis into a canonical form
    (destructuring-bind ((source-matrix source-v) (target-matrix target-v))
        (list (canonicalize-wf (nondestructively-apply-matrix-to-vector +edag-basis+
                                                                        source-wf))
              (canonicalize-wf (nondestructively-apply-matrix-to-vector +edag-basis+
                                                                        target-wf)))
      ;; check that source-v and target-v are close to equal
      (unless (and (double= (aref source-v 0) (aref target-v 0))
                   (double= (aref source-v 1) (aref target-v 1))
                   (double= (aref source-v 2) (aref target-v 2))
                   (double= (aref source-v 3) (aref target-v 3)))
        ;; NOTE: this is a hard return which escapes the implicit WITH-INST,
        ;;       which might even have PREFIX-CIRCUIT junk in it
        (return-from state-prep-2Q-compiler
          (state-prep-trampolining-compiler instr)))
      ;; write t^dag s as a member of SU(2) x SU(2)
      (multiple-value-bind (c1 c0)
          (convert-su4-to-su2x2
           (m* +e-basis+
               (magicl:conjugate-transpose target-matrix)
               source-matrix
               +edag-basis+))
        ;; write out the instructions
        (inst "LHS-state-prep-gate" c1 (second (application-arguments instr)))
        (inst "RHS-state-prep-gate" c0 (first (application-arguments instr)))))))

(defun coerce-to-complex-double-vector (elts)
  (map '(vector (complex double-float))
       (lambda (val) (coerce val '(complex double-float)))
       elts))

(defun schmidt-decomposition (phi num-a num-b)
  "Given a wavefunction PHI containing subystems of size NUM-A and NUM-B qubits, compute the Schmidt decomposition of PHI."
  ;; Returns c, U, V, where c is a vector and U, V are unitary matrices (of dimensions
  ;; NUM-A and NUM-B respectively) such that
  ;;
  ;;    PHI = \sum_{i} c_i U_i V _i
  ;;
  ;; where U_i, V_i denotes the ith column of the matrix U, V respectively.
  (assert (= (qubit-count phi) (+ num-a num-b)))
  (when (listp phi)
    (setf phi (coerce-to-complex-double-vector phi)))
  (let* ((size-a (expt 2 num-a))
         (size-b (expt 2 num-b))
         ;; tranpose here to make this row-major
         (reshaped (magicl:transpose (magicl:make-matrix :rows size-a
                                                         :cols size-b
                                                         :data phi))))
    (multiple-value-bind (u d vt)
        (magicl:svd reshaped)
      (values (coerce-to-complex-double-vector (magicl:matrix-diagonal d))
              u
              (magicl:transpose vt)))))

(defun state-prep-4q (wf q0 q1 q2 q3 &key reversed)
  "Produce instructions to prepare the wavefunction WF with respect to qubits Q0 Q1 Q2 Q3, starting from the zero state. If REVERSED is T, the instructions instead prepare the zero state from WF."
  ;; The following is from arXiv:1003.5760
  (assert (= 4 (qubit-count wf)))
  ;; each function in the following FLET is responsible for handling the REVERSED
  ;; flag on its own
  (flet ((state-prep-2q (source target qubit-indices)
           (when reversed
             (rotatef source target))
           (make-instance 'state-prep-application
                          :source-wf source
                          :target-wf target
                          :arguments (mapcar #'qubit qubit-indices)))
         (cnot (a b)
           (build-gate "CNOT" () a b))
         (2q-evolution (U a b)
           (anon-gate "STATE-2Q"
                      (if reversed (magicl:dagger U) U)
                      a b)))
    (multiple-value-bind (c U V)
        (schmidt-decomposition wf 2 2)
      (let ((instrs
              (list
               ;; prepare on qubits 2,3
               (state-prep-2q (build-ground-state 2)
                              c
                              (list q2 q3))
               ;; entangle
               (cnot q2 q0)
               (cnot q3 q1)

               ;; apply unitaries, transposing qubits to account for LSB <-> MSB mismatch
               (2q-evolution U q3 q2)
               (2q-evolution V q1 q0))))
        (if reversed
            (reverse instrs)
            instrs)))))

(define-compiler state-prep-4Q-compiler
    ((instr (_ _ q0 q1 q2 q3)
            :where (typep instr 'state-prep-application)))
  "Compiler for STATE-PREP-APPLICATION instances that target a single qubit."
  (let ((source-wf (vector-scale
                    (/ (norm (coerce (state-prep-application-source-wf instr) 'list)))
                    (coerce (state-prep-application-source-wf instr) 'list)))
        (target-wf (vector-scale
                    (/ (norm (coerce (state-prep-application-target-wf instr) 'list)))
                    (coerce (state-prep-application-target-wf instr) 'list))))
    (finish-compiler
     (append
      ;; prepare source -> zero state
      (state-prep-4q source-wf q0 q1 q2 q3
                     :reversed t)
      ;; prepare zero state -> target
      (state-prep-4q target-wf q0 q1 q2 q3)))))


(define-compiler state-prep-trampolining-compiler
    ((instr :where (typep instr 'state-prep-application)))
  "Recursive compiler for STATE-PREP-APPLICATION instances. It's probably wise to use this only if the state preparation instruction targets at least two qubits."
  (flet ((calculate-state-prep-angles (wf &optional (prefactor 1.0d0))
           ;; computes UCR angles for a circuit satisfying
           ;; UCRY(phi) UCRZ(theta) |wf> = |wf'> (x) |0>.
           ;; returns (values phi theta wf').
           ;;
           ;; TODO: This probably isn't the best thing to in the case that both
           ;;       b0 and b1 are zero, since that means it doesn't matter what
           ;;       angle they're acted on by, and it's possible that a wise
           ;;       choice of angle can cause early UCR compilation collapse.
           (loop :with b0 :and b1
                 :for index :below (array-total-size wf) :by 2
                 :do (setf b0 (aref wf index))
                     (setf b1 (aref wf (1+ index)))
                 :collect (constant (* prefactor -1 (- (phase b1) (phase b0))))
                   :into theta
                 :collect (constant (* prefactor (* -2 (atan (abs b1) (abs b0)))))
                   :into phi
                 :collect (* (sqrt (+ (expt (abs b0) 2) (expt (abs b1) 2)))
                             (cis (/ (+ (phase b1) (phase b0)) 2)))
                   :into reduced-bs
                 :finally
                    (return-from calculate-state-prep-angles
                      (values phi
                              theta
                              (make-array (length reduced-bs)
                                          :element-type '(complex double-float)
                                          :initial-contents reduced-bs))))))
    ;; compute the instructions to move from the source to the ground state
    ;; and from the ground state to the target
    (multiple-value-bind (source-to-zero-Y-angles source-to-zero-Z-angles source-wf)
        (calculate-state-prep-angles (state-prep-application-source-wf instr))
      (multiple-value-bind (zero-to-target-Y-angles zero-to-target-Z-angles target-wf)
          (calculate-state-prep-angles (state-prep-application-target-wf instr) -1.0d0)
        (let* ((ucr-arguments (reverse (application-arguments instr)))
               (UCR-Y (repeatedly-fork (named-operator "RY") (1- (length ucr-arguments))))
	       (UCR-Z (repeatedly-fork (named-operator "RZ") (1- (length ucr-arguments)))))
          ;; build the resulting circuit
	  ;; move from source Z to ground Z
	  (inst* UCR-Z source-to-zero-Z-angles ucr-arguments)
	  ;; move from source Y to ground Y
	  (inst* UCR-Y source-to-zero-Y-angles ucr-arguments)
	  ;; trampoline
	  (inst (make-instance 'state-prep-application
			       :source-wf source-wf
			       :target-wf target-wf
			       :arguments (rest (application-arguments instr))))
	  ;; move from ground to target Y
	  (inst* UCR-Y zero-to-target-Y-angles ucr-arguments)
	  ;; move from ground to target Z
	  (inst* UCR-Z zero-to-target-Z-angles ucr-arguments))))))


;; this decomposition algorithm is based off of Section 4 of /0406176, our old QSC favorite
(defun state-prep-compiler (instr &key (target ':cz))
  "Compiles a STATE-PREP-APPLICATION instance by intelligently selecting one of the special-case compilation routines above."
  (declare (ignore target))

  (unless (typep instr 'state-prep-application)
    (give-up-compilation))

  (case (length (application-arguments instr))
    (1
     (state-prep-1Q-compiler instr))
    (2
     (state-prep-2Q-compiler instr))
    (4
     (state-prep-4Q-compiler instr))
    (otherwise
     (state-prep-trampolining-compiler instr))))
