(in-package #:cl-quil.chip-builder)

(defun 2q? (qcomplex)
  (check-type qcomplex symbol)
  (find #\- (string qcomplex)))

(defmacro chip (name &body isa)
  "Build a CHIP-SPECIFICATION from the provided ISA.

ISA is of the form

    (qcomplex
     (instruction-constructor
      instruction-constructor
      ...
     ))

where qcomplex is either a number indicating a qubit, or a pair of qubits (separated by a hyphen) indicating an edge between qubits. The instruction-constructor are functions that return a HASH-TABLE representing gate information for the qcomplex. For example, to build a 2Q chip with standard 1Q instructions and a CZ

    (chip \"humber road chippy\"
      (0
       (measure _ :fidelity 0.999)
       (rz _ _ :fidelity 0.99)
       (rx pi _ :fidelity 0.98)
       (rx -pi _ :fidelity 0.98)
       (rx pi/2 _ :fidelity 0.98)
       (rx -pi/2 _ :fidelity 0.98)
       (rx pi _ :fidelity 0.98))
      (1
       (measure _ :fidelity 0.999)
       (rz _ _ :fidelity 0.999)
       (rx pi _ :fidelity 0.98)
       (rx -pi _ :fidelity 0.98)
       (rx pi/2 _ :fidelity 0.98)
       (rx -pi/2 _ :fidelity 0.98))
      (0-1
       (cz 0 1 :fidelity 0.99)))

where the underscore (_) is a stand-in for a concrete value that matches anything.

Alternatively, providing (defaults) to the qcomplex will fill in some default gates:

  - 1Q: MEASURE, {RZ(alpha) | alpha ∈ [0, 2π)}, {RX(beta) | beta ∈ {0, ±π/2, ±π}}

  - 2Q: CZ

The above becomes

    (chip \"humber road chippy\"
      (0 (defaults))
      (1 (defaults))
      (0-1 (defaults)))

"
  `(let ((chip (make-hash-table :test #'equal))
         (isa (make-hash-table :test #'equal))
         (1q (make-hash-table :test #'equal))
         (2q (make-hash-table :test #'equal))
         (_ "_"))
     (setf (gethash "isa" chip) isa)
     (setf (gethash "description" isa) ,name)
     (setf (gethash "1Q" isa) 1q)
     (setf (gethash "2Q" isa) 2q)
     (labels ((gate-information (name &key parameters arguments duration fidelity)
                (a:alist-hash-table
                 `(("operator" . ,name)
                   ("arguments" . ,arguments)
                   ("parameters" . ,parameters)
                   ("duration" . ,duration)
                   ("fidelity" . ,fidelity))
                 :test #'equal))
              (measure (qubit &key (fidelity 0.95) duration)
                (a:alist-hash-table
                 `(("operator" . "MEASURE")
                   ("qubit" . ,qubit)
                   ("duration" . ,duration)
                   ("fidelity" . ,fidelity))
                 :test #'equal))
              (rz (parameter qubit &key (fidelity 1.0) duration)
                (gate-information "RZ" :parameters (list parameter)
                                       :arguments (list qubit)
                                       :fidelity fidelity
                                       :duration duration))
              (rx (parameter qubit &key (fidelity 0.995) duration)
                (gate-information "RX" :parameters (list parameter)
                                       :arguments (list qubit)
                                       :fidelity fidelity
                                       :duration duration))
              (cz (control target &key (fidelity 0.95) duration)
                (gate-information "CZ" :arguments (list control target)
                                       :fidelity fidelity
                                       :duration duration))
              (cphase (parameter control target &key (fidelity 0.93) duration)
                (gate-information "CPHASE" :parameters (list parameter)
                                           :arguments (list control target)
                                           :fidelity fidelity
                                           :duration duration))
              (xy (parameter control target &key (fidelity 0.98) duration)
                (gate-information "XY" :parameters (list parameter)
                                       :arguments (list control target)
                                       :fidelity fidelity
                                       :duration duration))
              (defaults (n-qubits)
                (case n-qubits
                  (1 (list (measure _) (rz _ _) (rx 0 _) (rx pi/2 _) (rx -pi/2 _)
                           (rx pi _) (rx -pi _)))
                  (2 (list (cz _ _))))))
       (progn
         ,@(loop :for (qcomplex . gates) :in isa
                 :for qsymbol := (intern (format nil "~a" qcomplex))
                 :collect `(setf (gethash (string ',qsymbol)
                                          ,(if (2q? qsymbol) `2q `1q))
                                 (a:alist-hash-table '(("gates" . ())) :test #'equal)) :into forms
                 :append
                    (loop :for gate :in gates :collect
                             `(setf (gethash "gates"
                                             (gethash (string ',qsymbol)
                                                      ,(if (2q? qsymbol) `2q `1q)))
                                    (append
                                     (gethash "gates"
                                              (gethash (string ',qsymbol)
                                                       ,(if (2q? qsymbol) `2q `1q)))
                                     ,(if (string= "DEFAULTS" (string (car gate)))
                                          `(defaults ,(if (2q? qsymbol) 2 1))
                                          gate))))
                 :into forms
                 :finally (return forms)))
       (quil::qpu-hash-table-to-chip-specification chip))))
