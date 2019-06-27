;;;; solovay-kitaev.lisp
;;;;
;;;; Authors: Andrew Shi, Mark Skilbeck

(in-package :cl-quil)

;;; This file contains an implementation of the Solovay-Kitaev
;;; algorithm described in https://arxiv.org/pdf/quant-ph/0505030.pdf,
;;; used to approximately decompose arbitrary unitaries to a finite
;;; set of basis gates.

(defparameter +c-approx+ (* 4 (sqrt 2)))
(defparameter +c-gc+ (/ (sqrt 2)))

(defclass decomposer ()
  ((basis-gates :reader basis-gates
                :initarg :basis-gates
                :type (list integer)
                :initform (error ":BASIS-GATES is a required initarg to DECOMPOSER.")
                :documentation "Set of basic gates to decompose to.")
   (num-qubits :reader num-qubits
               :initarg :num-qubits
               :type non-negative-fixnum
               :initform (error ":NUM-QUBITS is a required initarg to DECOMPOSER.")
               :documentation "Number of qubits the operators of this
               decomposer act on.")
   (epsilon0 :reader epsilon0
             :initarg :epsilon0
             :type double-float
             :initform (error ":EPSILON0 is a required initarg to DECOMPOSER.")
             :documentation "Parameter controlling the density of
               base-approximation unitaries for this
               decomposer. Specifically, every unitary operator on
               NUM-QUBITS should be within EPSILON0 of some unitary in
               BASE-APPROXIMATIONS.")
   (base-approximations :accessor base-approximations
                        :initarg :base-approximations
                        :initform nil
                        :documentation "A set of base approximations
                        such that every unitary operator on NUM-QUBITS
                        is within EPSILON0 of some unitary in the
                        set."))
  (:documentation "A decomposer which uses the Solovay-Kitaev
  algorithm to approximately decompose arbitrary unitaries to a finite
  set of basis gates."))

(defun make-decomposer (basis-gates num-qubits epsilon0)
  "Initializer for a unitary decomposer."
  (make-instance 'decomposer
                 :basis-gates basis-gates
                 :num-qubits num-qubits
                 :epsilon0 epsilon0
                 :base-approximations (generate-base-approximations basis-gates num-qubits epsilon0)))

;; Will be somewhat complicated/tedious
(defun generate-base-approximations (basis-gates num-qubits epsilon0))

(defstruct commutator
  (v :type (or commutator symbol-list))
  (w :type (or commutator symbol-list)))

(defun multiply-commutator ((comm commutator)))

(defun expand-commutator ((comm commutator))
  (let ((v (commutator-v comm))
        (w (commutator-w comm)))
    (append v w (dagger v) (dagger w))))

(defun dagger (gate-seq)
  (reverse (mapcar #'inverse gate-seq)))

(defun matrix-trace (m)
  (loop :for i :below (magicl:matrix-cols m)
        :sum (magicl:ref m i i)))

(defun fidelity (m)
  (let ((p (* 2 (log (magicl:matrix-cols m) 2))))
    (/ (+ (expt (abs (matrix-trace m)) 2) p)
       (+ (expt p 2) p))))

(defun d (u s)
  (- 1 (fidelity (magicl:multiply-complex-matrices
                  (magicl:conjugate-transpose s)
                  u))))

;; May not be needed, if accessing from the decomposer object directly
(defun base-approx (u)
  "Base case approximation for a unitary U.")

;; Don't fully understand this part yet
(defun gc-decompose (u)
  "Find the balanced group commutators V and W for a unitary U.")

(defun sk-iter (decomposer unitary depth)
  "An approximation iteration within the Solovay-Kitaev algorithm at a
given depth."
  (if (= depth 0)
      (base-approx unitary)
      (let* ((comm (gc-decompose unitary))
             (v )
             (w ))
        ;; do something
        )))

(defun decompose (decomposer unitary epsilon)
  "Decomposes a unitary into a sequence of gates from a finite set of
basis gates, such that the resulting decomposition is within EPSILON
of the original unitary."
  (let* ((eps0 (epsilon0 decomposer))
         (depth (ceiling (log (/ (log (* epsilon ))
                                (log)))
                        (log (/ 3 2)))))
    (sk-iter decomposer unitary depth)))
