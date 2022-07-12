;;;; qubits-needed.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:cl-quil/frontend)

;;; This analysis counts the number of qubits needed to run a program.

(defgeneric %max-qubit (isn)
  (:documentation "Helper function to determine the maximum qubit index involved in an instruction.")
  (:method ((isn instruction))
    -1)
  (:method ((isn jump-target))
    -1)
  (:method ((isn measurement))
    (qubit-index (measurement-qubit isn)))
  (:method ((isn application))
    (reduce #'max (application-arguments isn)
            :initial-value -1
            :key (lambda (arg)
                   (if (qubit-p arg)
                       (qubit-index arg)
                       -1))))
  (:method ((isn reset-qubit))
    (qubit-index (reset-qubit-target isn))))

(defun qubits-needed (parsed-prog)
  "Count the number of qubits needed by the program PARSED-PROG."
  (1+ (reduce #'max (parsed-program-executable-code parsed-prog)
              :key #'%max-qubit
              :initial-value -1)))
