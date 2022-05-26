;;;; src/discrete/discrete-chip.lisp
;;;;
;;;; Author: Robert Smith, A.J. Nyquist

(in-package #:cl-quil.discrete)

;;; A "discrete gate set" is purposefully vague, since the definition
;;; may change over time. For now, though, the gate set is {H, S, T,
;;; CNOT}, sometimes known as "Clifford+T".

(alexandria:define-constant +discrete-gate-names+ '("H" "S" "T" "CNOT")
  :test 'equalp
  :documentation "The names of the gates of the supported discrete gate set.")

;;; TODO: Maybe take some q:: symbols and expose them from QUIL.SI.

(defun build-discrete-qubit (q &key (compile-fast nil))
  "Build a qubit hardware object numbered Q that supports the a discrete gate
set and measurement. When COMPILE-FAST, will trick quilc into skipping certain
compilation routines."
  (check-type q unsigned-byte)
  (let ((obj (q::make-hardware-object :order 0)))
    (setf (gethash (q::make-measure-binding :qubit q
                                            :target '_)
                   (q::hardware-object-gate-information obj))
          (q::make-gate-record :duration 2000 :fidelity 0.90d0))
    (setf (gethash (q::make-measure-binding :qubit '_)
                   (q::hardware-object-gate-information obj))
          (q::make-gate-record :duration 2000 :fidelity 0.90d0))
    (dolist (op '("H" "S" "T"))
      (setf (gethash (q::make-gate-binding :operator (q:named-operator op)
                                           :parameters nil
                                           :arguments (list q))
                     (q::hardware-object-gate-information obj))
            (q::make-gate-record :duration 50
                                 :fidelity q::+near-perfect-fidelity+)))
    ;; Speed up compilation by skipping numerous expensive calls
    ;; With such poor fidelity, quilc will still expand RZs in the end
    (when compile-fast
      (setf (gethash (q::make-gate-binding :operator (q:named-operator "RZ")
                                           :parameters (list '_)
                                           :arguments (list q))
                     (q::hardware-object-gate-information obj))
            (q::make-gate-record :duration 9999
                                 :fidelity 0.1)))
    obj))

(defun install-discrete-link-onto-chip (chip-spec i j)
  "Install a Clifford link between hardware objects I and J on the chip CHIP."
  (q::install-link-onto-chip chip-spec i j :architecture ':cnot))

(defun build-discrete-linear-chip (n &key (compile-fast nil))
  "Build a linear array of N discrete qubits connected by a Clifford gate. See `build-discrete-qubit' for COMPILE-FAST."
  (let ((chip-spec (q::make-chip-specification
                    :generic-rewriting-rules (coerce (q::global-rewriting-rules) 'simple-vector))))
    (q::install-generic-compilers chip-spec ':cnot)
    (dotimes (q n)
      (q::adjoin-hardware-object
       (build-discrete-qubit q :compile-fast compile-fast) chip-spec))
    (dotimes (i (1- n))
      (install-discrete-link-onto-chip chip-spec i (1+ i)))
    (q:warm-hardware-objects chip-spec)))

(chips:install-chip-builder "4Q-cliffordt" (lambda () (build-discrete-linear-chip 4 :compile-fast t)))
