;;;; qs-compile.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; Implements the Quantum Shannon Decomposition of Shende et al. (/0406176) as
;;;; a gate compilation routine.  Targets UCRs.  Intended to supplant CSC.

(in-package #:cl-quil)

(define-compiler qs-compiler (instr)
  "Performs Quantum Shannon Compilation, emitting a list of anonymous gates and UCR instructions that describe an equivalent circuit."
  (unless (or (>= (length (application-arguments instr)) 3)
              (adt:match operator-description (application-operator instr)
                ((named-operator name) (not (find name (standard-gate-names) :test #'string=)))
                (_ t)))
    (give-up-compilation))
  
  (let ((matrix (gate-matrix instr)))
    (unless matrix
      (give-up-compilation))
    (let* ((row-count (magicl:matrix-rows matrix))
           (prefactor (expt (magicl:det matrix) (- (/ row-count))))
           (m (magicl:scale prefactor matrix)))
      ;; it starts the same as CSC: build the Cosine-Sine decomposition
      (multiple-value-bind (u0 u1 v0 v1 thetas) (funcall *lapack-csd* m (/ row-count 2) (/ row-count 2))
        ;; rebalance the u and v matrices so that u0 (+) u1 and v0 (+) v1
        ;; are special unitary.
        (let* ((neg-nth-root-detu
                 (expt (* (magicl:det u0) (magicl:det u1))
                       (/ -1 row-count)))
               (u0 (magicl:scale neg-nth-root-detu u0))
               (u1 (magicl:scale neg-nth-root-detu u1))
               (v0 (magicl:scale (/ neg-nth-root-detu) v0))
               (v1 (magicl:scale (/ neg-nth-root-detu) v1))
               ;; now we *also* want u0, u1, v0, v1 to be special unitary. the
               ;; price we pay for this is a Z rotation. calculate their angle
               ;; values now...
               (Lphi (realpart (/ (log (magicl:det u0)) #C(0 1) (/ row-count 2))))
               (Rphi (realpart (/ (log (magicl:det v0)) #C(0 1) (/ row-count 2))))
               ;; ... and then do the rescaling.
               (u0 (magicl:scale (cis (- Lphi)) u0))
               (u1 (magicl:scale (cis Lphi) u1))
               (v0 (magicl:scale (cis (- Rphi)) v0))
               (v1 (magicl:scale (cis Rphi) v1)))
          ;; now we convert the multiplexed operators u0 (+) u1, v0 (+) v1
          ;; into ordinary operators with a UCRZ.  we are going off of the eqn
          ;;    u0 (+) u1 = (uL (+) uL) (uD (+) uD^*) (uR (+) uR).
          ;; the idea is to set uL to be a diagonalizing unitary of u0, and uD^2
          ;; is its diagonal matrix of eigenvalues. equivalently, once uL is
          ;; chosen in this manner, we can compute
          ;;    uD = sqrt(uL^* u0 u1^* uL),
          ;; and then uR can be deduced from this by
          ;;    uR = u0 uL^* uD^*.
          (multiple-value-bind (evals-u uL)
              (magicl:eig (magicl:multiply-complex-matrices u0 (magicl:conjugate-transpose u1)))
            (multiple-value-bind (evals-v vL)
                (magicl:eig (magicl:multiply-complex-matrices v0 (magicl:conjugate-transpose v1)))
              ;; if the eigenspaces of u0 v1^* or v0 v1^* are pluridimensional,
              ;; then the columns of uL and vL are not guaranteed to form an
              ;; orthonormal set---i.e., they are not guaranteed to be unitary.
              ;; this is OK: the eigenspaces themselves are orthogonal, so we
              ;; can perform gram-schmidt orthogonalization to form a unitary
              ;; matrix that will serve us just as well.
              (setf uL (orthonormalize-matrix uL))
              (setf vL (orthonormalize-matrix vL))
              ;; to compute uD and vD, we take the square roots of the diagonal eigenvalue matrices
              (setf evals-u (mapcar #'sqrt evals-u))
              (setf evals-v (mapcar #'sqrt evals-v))
              ;; but taking square roots isn't well-defined, so we might end up
              ;; with a non-special unitary matrix by doing this. when this
              ;; happens, we pick a different square root representative.
              (when (> 0 (realpart (apply '* evals-u)))
                (setf (first evals-u) (- (first evals-u))))
              (when (> 0 (realpart (apply '* evals-v)))
                (setf (first evals-v) (- (first evals-v))))
              (let* ((uL (magicl:scale (expt (magicl:det uL) (- (/ (magicl:matrix-rows uL)))) uL))
                     (vL (magicl:scale (expt (magicl:det vL) (- (/ (magicl:matrix-rows vL)))) vL))
                     (uR (reduce #'magicl:multiply-complex-matrices
                                 (list (magicl:diag (/ row-count 2)
                                                    (/ row-count 2)
                                                    evals-u)
                                       (magicl:conjugate-transpose uL)
                                       u1)))
                     (vR (reduce #'magicl:multiply-complex-matrices
                                 (list (magicl:diag (/ row-count 2)
                                                    (/ row-count 2)
                                                    evals-v)
                                       (magicl:conjugate-transpose vL)
                                       v1)))
                     ;; some convenient shorthand for inst below
                     (control (first (application-arguments instr)))
                     (rest (rest (application-arguments instr)))
                     (UCR-Y (repeatedly-fork (named-operator "RY") (length rest)))
		     (UCR-Z (repeatedly-fork (named-operator "RZ") (length rest))))
                ;; we now have the equality
                ;; m = (u0 (+) u1) Z(Lphi) Y(thetas) Z(Rphi) (v0 (+) v1)
                ;;   = uL Z(evals-u) uR Z(Lphi) Y(thetas) Z(Rphi) vL Z(evals-v) vR
                ;; which we use to build the output circuit.
		(inst* "QSC-VR" vR                 rest)
		(inst* UCR-Z    (mapcar (lambda (x) (constant (* -2 (phase x)))) evals-v)
		                (append rest (list control)))
		(inst* "QSC-VL" vL                 rest)
		(inst  "RZ"     (list (* -2 Rphi)) control)
		(inst* UCR-Y    (mapcar (lambda (x) (constant (* 2 x))) thetas)
		                (append rest (list control)))
		(inst  "RZ"     (list (* -2 Lphi)) control)
		(inst* "QSC-UR" uR                 rest)
		(inst* UCR-Z    (mapcar (lambda (x) (constant (* -2 (phase x)))) evals-u)
		                (append rest (list control)))
		(inst* "QSC-UL" uL                 rest)))))))))
