;;;; src/discrete/discrete-chip.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil.discrete)

;;; A "discrete gate set" is purposefully vague, since the definition
;;; may change over time. For now, though, the gate set is {H, S, T,
;;; CNOT}, sometimes known as "Clifford+T".

(alexandria:define-constant +discrete-gate-names+ '("H" "S" "T" "CNOT")
  :test 'equalp
  :documentation "The names of the gates of the supported discrete gate set.")

;;; TODO: Maybe take some q:: symbols and expose them from QUIL.SI.

(defun build-discrete-qubit (q)
  "Build a qubit hardware object numbered Q that supports the a discrete gate set and measurement."
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
    obj))

(defun install-discrete-link-onto-chip (chip-spec i j)
  "Install a Clifford link between hardware objects I and J on the chip CHIP."
  (q::install-link-onto-chip chip-spec i j :architecture ':cnot))

(defun build-discrete-linear-chip (n)
  "Build a linear array of N discrete qubits connected by a Clifford gate."
  (let ((chip-spec (q::make-chip-specification
                    :generic-rewriting-rules (coerce (q::global-rewriting-rules) 'simple-vector))))
    (q::install-generic-compilers chip-spec ':cnot)
    (loop :for q :below n
          :do (q::adjoin-hardware-object (build-discrete-qubit q) chip-spec))
    (dotimes (i (1- n))
      (install-discrete-link-onto-chip chip-spec i (1+ i)))
    (q:warm-hardware-objects chip-spec)))
