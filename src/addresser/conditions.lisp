(in-package #:cl-quil)

(define-condition user-program-incompatible-with-chip (serious-condition)
  ()
  (:documentation "This is signaled when the user program is incompatible with the chip in some way."))

(define-condition chip-insufficient-qubits (user-program-incompatible-with-chip)
  ((needed :reader chip-insufficient-qubits-needed :initarg :needed)
   (available :reader chip-insufficient-qubits-available :initarg :available))
  (:report (lambda (condition stream)
             (format stream "User program incompatible with chip: qubit index ~A used and ~A available."
                     (chip-insufficient-qubits-needed condition)
                     (chip-insufficient-qubits-available condition)))))

(define-condition naive-rewiring-crosses-chip-boundaries (user-program-incompatible-with-chip)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "User program incompatible with chip: naive rewiring crosses chip component boundaries."))))

(define-condition connected-components-incompatible (user-program-incompatible-with-chip)
  ((program-connected-components :reader incompatible-program-ccs :initarg :program-connected-components)
   (chip-connected-components :reader incompatible-chip-ccs :initarg :chip-connected-components))
  (:report (lambda (condition stream)
             (format stream "User program incompatible with chip: The program ~
    uses operations on qubits that cannot be logically mapped onto the chip ~
    topology. This set of qubits in the program cannot be assigned to qubits on ~
    the chip compatibly under a greedy connected component allocation scheme: ~
    ~A. The chip has the components ~A."
                     (incompatible-program-ccs condition)
                     (incompatible-chip-ccs condition)))))
