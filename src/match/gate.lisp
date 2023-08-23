;;;; gate.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; gate is an interface that holds the default methods to be
;;;; replaced by subclasses that stores the matrices and qubits
;;;; operated on by gates in the circuit

;; This is an interface for the gates held in the circuit
(defclass gate ()
  ((qubits
    :initarg :qubits
    :reader qubits
    :documentation "An ordered list of qubits stored as nonnegative integers"))
  (:default-initargs
   :qubits (error "Must supply qubits."))
  (:documentation "An interface for the gates held in a circuit, a subclass must override all methods"))

;;; The following are default methods. For best results with
;;; pattern-match, replace the following methods with more precise
;;; versions when creating a subclass of gate

;; Gets whether two gates are identical
(defmethod gate-matched-p ((gate gate) (other gate))
  (error "Subclasses of gate must override gate-matched-p."))

;; Gets whether two gates commute
(defmethod gates-commute-p ((gate gate) (other gate))
  (error "Subclasses of gate must override gates-commute-p."))

;; Gets whether two nodes perform an equal operation, albeit possibly
;; on different qubits
(defmethod equal-gate-operation-p ((gate gate) (other gate))
  (error "Subclasses of gate must override equal-gate-operation."))

;; Gets whether the gate affects the passed qubit in a consequential
;; manner
(defmethod gate-act-consequentialy-on-qubit-p ((gate gate) (qubit integer)) 
  (error "Subclasses of gate must override gate-act-consequentialy-on-qubit-p."))

;; Gets the inverse of the given gate
(defgeneric gate-inverse (gate)
  (:documentation "Gets the inverse of the given gate")
  (:method ((gate gate))
    (error "Subclasses of gate must override gate-inverse.")))

;; Builds a gate of the same class as the passed gate and holding the
;; same data, but with a replaced set of qubits
(defgeneric permute-qubits (gate qubits)
  (:documentation "Builds a gate of the same class as the passed gate and holding the same data, but with a replaced set of qubits")
  (:method ((gate gate) qubits)
    (error "Subclasses of gate must override permute-qubits.")))

;; Gets the total cost of running the series of gates
(defgeneric calculate-cost (gate gates)
  (:documentation "Gets the total cost of running the series of gates")
  (:method ((gate gate) gates)
    (error "Subclasses of gate must override calculate-cost.")))

;; A wrapper for calculate-cost that ensures that if the length of the
;; gates is 0, only 0 is returened and otherwise calculate-cost is
;; called
(defun gate-cost (gate-list)
  (cond ((= (length gate-list) 0)
         0)
        (T
         (calculate-cost (first gate-list) gate-list))))

;; Gets whether two qubits are the same by comparing comparing two
;; integer representations of qubits
(defgeneric same-qubit-p (qubit other)
  (:documentation "Checks whether two qubits are the same")
  (:method ((qubit integer) (other integer))
    (= qubit other)))
