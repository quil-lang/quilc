;;;; test-gate.lisp
;;;;
;;;; Author: Brennen Hill

(in-package #:cl-quil.match)

;;;; A subclass of gate used for testing

;; The subclass of gate, which is used to run tests It represents an N
;; by N matrix, where N is equal to (expt 2 (length qubits))
(defclass test-gate (gate)
  ((matrix
    :initarg :matrix
    :reader matrix
    :documentation "The matrix in the computational basis"))
  (:default-initargs
   :matrix (error "Must supply a matrix."))
  (:documentation "A gate used for testing pattern-match and pattern-replace"))

;; Defaults to checking that they operate on the same qubits and hold
;; the same matrix
(defmethod gate-matched-p ((gate test-gate) (other gate))
  (and
   (= (length (qubits gate)) (length (qubits other)))
   (every #'same-qubit-p (qubits gate) (qubits other))
   (equal-gate-operation-p gate other)))

;; Defaults to checking whether they operate on any of the same qubits
(defmethod gates-commute-p ((gate test-gate) (other test-gate))
  (let ((a-qubits (coerce (qubits gate) 'list))
        (b-qubits (coerce (qubits other) 'list)))
    (cond
      ((null (intersection a-qubits b-qubits))
       T)
      (T
       ;; NOTE: Change this later, as it is not true. If any qubits
       ;; are in common, let's just assume they don't commute
       NIL))))

;; Defaults to comparing the gates' matrices with equalp
;;
;; NOTE: equalp works for this application, but needs to be improved
;; upon (non-critical)
;;
;; NOTE: Allow equal-gate-operation to work when dimensions are
;; different (non-critical)
(defmethod equal-gate-operation-p ((gate test-gate) (other test-gate))
  (equalp (matrix gate) (matrix other))) 

;; Defaults to checking whether the qubit is one of the qubits
;; operated on by the gate
;;
;; NOTE: Check the action of the gate on the qubit, make sure the
;; qubit is actually affected, unlike with identity
(defmethod gate-act-consequentialy-on-qubit-p ((gate test-gate) (qubit integer)) 
  (if (find qubit (qubits gate)) T NIL))

;; Defaults to returning the original gate
;;
;; NOTE: the current behavior is not accurate, as it simply adds a
;; negative sign to the representation of the matrix, which is assumed
;; to be a symbol
(defmethod gate-inverse ((gate test-gate))
  (make-instance 'test-gate
                 :qubits (qubits gate)
                 :matrix (read-from-string
                          (concatenate 'string "-"
                                       (symbol-name (matrix gate))))))

;; Defaults to building a new Gate with the same matrix and the passed
;; qubits
(defmethod permute-qubits ((gate test-gate) qubits)
  (make-instance 'test-gate
                 :qubits qubits
                 :matrix (matrix gate)))


;; Defaults to returning the number of gates
;;
;; NOTE: The current behavior does not reflect how most gates have a
;; different cost to run
(defmethod calculate-cost ((gate test-gate) gates)
  (length gates))
