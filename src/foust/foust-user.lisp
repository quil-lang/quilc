;;;; foust-user.lisp
;;;;
;;;; Author: Yarin Heffes

(defpackage #:cl-quil.foust-user
  (:use
   #:coalton
   #:coalton-prelude)
  (:import-from
   #:cl-quil.foust/graphviz
   #:graphviz)
  (:import-from
   #:cl-quil.foust
   #:releasing-foust
   #:preserving-foust)
  (:local-nicknames
   (#:gate #:cl-quil.foust/gate)))

(in-package #:cl-quil.foust-user)

(named-readtables:in-readtable coalton:coalton)

;; This package defines functions and variables for a user to
;; experiment with Foust and Fousting in a sandbox-like environment.

(coalton-toplevel

  (declare default-corrections (foust:Gate -> (Tuple (List foust:Gate) foust:Frame)))
  (define (default-corrections gate-g) (Tuple (singleton gate-g) (default)))

  (declare default-clifford-costs (foust:Gate -> IFix))
  (define (default-clifford-costs _) 0)

  (declare default-then-swap?? (UFix -> UFix -> Boolean))
  (define (default-then-swap?? _ _) False)

  (declare default-releasing-foust (foust:Circuit -> foust:Circuit))
  (define (default-releasing-foust circuit-c)
    (releasing-foust default-corrections default-clifford-costs default-then-swap?? circuit-c))

  (declare default-preserving-foust (foust:Circuit -> foust:Circuit))
  (define (default-preserving-foust circuit-c)
    (preserving-foust default-corrections default-clifford-costs default-then-swap?? circuit-c)))

(coalton-toplevel

  (define X (fn (qubit) (singleton (foust:PauliGate foust:X qubit))))
  (define Y (fn (qubit) (singleton (foust:PauliGate foust:Y qubit))))
  (define Z (fn (qubit) (singleton (foust:PauliGate foust:Z qubit))))

  (define H (fn (qubit) (singleton (foust:H foust:Plus foust:Y qubit))))
  (define S (fn (qubit) (singleton (foust:S foust:Z qubit))))
  (define Sdg (fn (qubit) (singleton (foust:SDag foust:Z qubit))))
  (define T (fn (qubit) (singleton (foust:R foust:Z (foust:Angle 1/8) qubit))))
  (define Tdg (fn (qubit) (singleton (foust:R foust:Z (foust:Angle -1/8) qubit))))

  (define CNOT (fn (control-qubit target-qubit) (singleton (foust:Controlled foust:X control-qubit target-qubit))))
  (define CZ (fn (qubit-one qubit-two) (singleton (foust:TQE foust:Z foust:Z qubit-one qubit-two))))
  (define iSWAP (fn (qubit-one qubit-two) (singleton (foust:iSwap qubit-one qubit-two))))
  (define SWAP (fn (qubit-one qubit-two) (singleton (foust:Swap qubit-one qubit-two))))

  (define RX (fn (qubit angle) (singleton (foust:R foust:X (foust:Angle angle) qubit))))
  (define RY (fn (qubit angle) (singleton (foust:R foust:Y (foust:Angle angle) qubit))))
  (define RZ (fn (qubit angle) (singleton (foust:R foust:Z (foust:Angle angle) qubit))))

  ;; 9 single qubit gates (7 Ts, 2 Cliffords), 6 TQEs.
  (define Toffoli (fn (control-qubit-one control-qubit-two target-qubit)
                    (mconcat (make-list (H target-qubit)
                                        (CNOT control-qubit-two target-qubit)
                                        (Tdg target-qubit)
                                        (CNOT control-qubit-one target-qubit)
                                        (T target-qubit)
                                        (CNOT control-qubit-two target-qubit)
                                        (Tdg target-qubit)
                                        (CNOT control-qubit-one target-qubit)
                                        (T control-qubit-two)
                                        (T target-qubit)
                                        (CNOT control-qubit-one control-qubit-two)
                                        (H target-qubit)
                                        (T control-qubit-one)
                                        (Tdg control-qubit-two)
                                        (CNOT control-qubit-one control-qubit-two)))))

  (define Prep (fn (qubit) (singleton (foust:Prep foust:Plus foust:Z foust:X qubit))))
  (define Meas (fn (qubit classical-variable) (singleton (foust:Meas foust:Plus foust:Z qubit classical-variable)))))

(coalton-toplevel

  ;; The following circuit is adapted from arXiv:2305.10966v2, Fig. 1.

  (define example-circuit-one (foust:make-circuit (mconcat (make-list (Prep 0)
                                                                (Prep 1)
                                                                (RX 0 1/3)
                                                                (H 1)
                                                                (CNOT 1 0)
                                                                (RX 0 1/5)
                                                                (CNOT 0 1)
                                                                (RX 0 1/7)
                                                                (Meas 0 0)
                                                                (Meas 1 1)))))

  ;; The following circuit is a Deutsch-Joza circuit for two qubits.

  (define example-circuit-two (foust:make-circuit (mconcat (make-list (Prep 0)
                                                                (Prep 1)
                                                                (X 1)
                                                                (H 0)
                                                                (H 1)
                                                                (CNOT 0 1)
                                                                (H 0)
                                                                (Meas 0 0)))))

  ;; The following circuit is a Grover Search on three qubits for the state |110>,
  ;; adapted from https://www.quantum-inspire.com/kbase/grover-algorithm/
  ;; 99 gates: 75 single qubit gates (28 Ts, 47 Cliffords), 24 TQEs.
  ;; Reduces to 44 gates: 28 single qubit gates (all pi/4 rotations), 16 TQEs.

  (define example-circuit-three
    (let ((oracle (mconcat (make-list (X 0)
                                      (H 2)
                                      (Toffoli 0 1 2)
                                      (X 0)
                                      (H 2))))
          (diffusion (mconcat (make-list (H 0) (H 1) (H 2)
                                         (X 0) (X 1) (X 2)
                                         (H 2) (Toffoli 0 1 2) (H 2)
                                         (X 0) (X 1) (X 2)
                                         (H 0) (H 1) (H 2)))))
      (foust:make-circuit (mconcat (make-list (Prep 0) (Prep 1) (Prep 2)
                                        (H 0) (H 1) (H 2)
                                        oracle
                                        diffusion
                                        oracle
                                        diffusion
                                        (Meas 0 0) (Meas 1 1) (Meas 2 2))))))

  ;; Test Preserve mode equivalence with the Toffoli and Swap gates.

  (define example-circuit-four (foust:make-circuit (Toffoli 0 1 2)))
  (define example-circuit-five (foust:make-circuit (mconcat (make-list (SWAP 0 1)))))
  (define example-circuit-six (foust:make-circuit (mconcat (make-list (iSWAP 0 1)))))

  ;; Test Release mode equivalence with the Toffoli applied to |110>.

  (define example-circuit-seven (foust:make-circuit (mconcat (make-list (Prep 0)
                                                                  (Prep 1)
                                                                  (Prep 2)
                                                                  (X 0)
                                                                  (X 1)
                                                                  (Toffoli 0 1 2)
                                                                  (Meas 2 0))))))
