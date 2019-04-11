;;;; relabeling.lisp

(in-package #:cl-quil)

(defun standard-qubit-relabeler (qubits-list)
  "Construct a standard qubit relabeler and its inverse.

A *relabeler* is a function that takes as input a logical qubit index and produces a relabeled (sometimes called *physical*) qubit index.

The *standard labeling* of qubits (in a context where qubits are supplied as arguments to a gate application) is in descending order to 0. As such, a given QUBITS-LIST (a b c ... y z) of length N would have a labeling of:

    a -> N - 1
    b -> N - 2
    c -> N - 3
    ...
    y -> 1
    z -> 0.

Qubits not present in QUBITS-LIST are mapped to themselves."
  (let ((length (length qubits-list)))
    (values (lambda (logical-qubit)
              (check-type logical-qubit qubit)
              (let ((p (position (qubit-index logical-qubit) qubits-list)))
                (if p
                    (qubit (- length p 1))
                    logical-qubit))))))
