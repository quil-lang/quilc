;;;; src/clifford/qubit-algebra.lisp
;;;;
;;;; Author: Nik Tezak

(in-package #:cl-quil/clifford)

;;; This file contains an abstract base class representing "qubit
;;; algebras", corresponding to algebras that have some correspondence
;;; with collections of qubits.

(defstruct (qubit-algebra (:conc-name nil))
  "A generic algebra involving objects specialized to NUM-QUBITS qubits.")

(defgeneric num-qubits (obj))

(defgeneric group-mul (a b)
  (:documentation
   "Composition (multiplication) of two group elements A and B."))

(defmethod group-mul :before ((a qubit-algebra) (b qubit-algebra))
  (unless (= (num-qubits a) (num-qubits b))
    (error "Unequal number of qubits.")))


(defgeneric group-inv (g)
  (:documentation
   "Compute the inverse of a group element."))


(defgeneric group-conj (a b)
  (:documentation
   "Conjugate b with a, i.e., compute a b a^{-1}."))

(defmethod group-conj :before ((a qubit-algebra) (b qubit-algebra))
  (unless (= (num-qubits a) (num-qubits b))
    (error "Unequal number of qubits.")))

(defmethod group-conj (a b)
  (group-mul (group-mul a b) (group-inv a)))

(defgeneric tensor-mul (a b)
  (:documentation
   "Form the tensor product of A and B."))


(defgeneric embed (a n idxs)
  (:documentation
   "Embed a qubit algebra element A into the N-qubit representation
of that algebra by mapping the indices of the degrees of freedom P
represents according to the list of indices IDXS."))

(defmethod embed :before (a n idxs)
  (assert (>= n (num-qubits a))
          (n a)
          "The embedded qubit algebra element A must have NUM-QUBITS <= N.")
  (assert (= (num-qubits a) (length idxs))
          (a idxs)
          "The length of the provided mapping of qubit indices IDXS must match the NUM-QUBITS of A."))
