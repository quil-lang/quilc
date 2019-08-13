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
  (format s "STATE-PREP[(~{~5$~^, ~}) -> (~{~5$~^, ~})] ~{~a~^ ~}"
          (coerce (state-prep-application-source-wf thing) 'list)
          (coerce (state-prep-application-target-wf thing) 'list)
          (mapcar (lambda (arg) (print-instruction arg nil))
                  (application-arguments thing))))


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
    (list (anon-gate "STATE-1Q" (m* matrix-target matrix-source) q))))


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
  "Calculates a special-orthogonal MATRIX that moves a unit-length VECTOR in C^4 into the form V = (a+bi c 0 0).  Returns (LIST MATRIX V)."
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
    (list matrix v)))


;; TODO: this should be made architecture-sensitive, with separate templates
;;       for ISWAP-based chips
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
              (setf abs-so-source (abs (segre-obstruction-2Q source-wf))))))))
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
        (append prefix-circuit
                (list (make-instance 'gate-application
                                     :gate c1
                                     :operator #.(named-operator "LHS-state-prep-gate")
                                     :arguments (list (second (application-arguments instr))))
                      (make-instance 'gate-application
                                     :gate c0
                                     :operator #.(named-operator "RHS-state-prep-gate")
                                     :arguments (list (first (application-arguments instr))))))))))

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
        (let ((ucr-arguments (reverse (application-arguments instr))))
          ;; build the resulting circuit
          (list
           ;; move from source Z to ground Z
           (apply #'build-UCR "RZ" source-to-zero-Z-angles ucr-arguments)
           ;; move from source Y to ground Y
           (apply #'build-UCR "RY" source-to-zero-Y-angles ucr-arguments)
           ;; trampoline
           (make-instance 'state-prep-application
                          :source-wf source-wf
                          :target-wf target-wf
                          :arguments (rest (application-arguments instr)))
           ;; move from ground to target Y
           (apply #'build-UCR "RY" zero-to-target-Y-angles ucr-arguments)
           ;; move from ground to target Z
           (apply #'build-UCR "RZ" zero-to-target-Z-angles ucr-arguments)))))))


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
    (otherwise
     (state-prep-trampolining-compiler instr))))
