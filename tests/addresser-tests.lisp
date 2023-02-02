;;;; addresser-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(deftest test-rewiring-shuffle ()
  (dolist (quil::*addresser-gates-swap-search-type* '(:a* :greedy-qubit :greedy-path))
    (not-signals error
      (let ((text "CNOT 2 0"))
        (is (quil::operator=
             (quil:parsed-program-to-logical-matrix
              (quil::parse-quil text))
             (quil:parsed-program-to-logical-matrix
              (quil::compiler-hook (quil::parse-quil text) (quil::build-8q-chip)
                                   :protoquil nil :rewiring-type ':naive))))))))

(deftest test-addresser-subclasses ()
  (let* ((chip (quil::build-8q-chip))
         (text "CNOT 0 1 ; CNOT 0 2 ; CNOT 1 2")
         (orig-mat (quil:parsed-program-to-logical-matrix (quil::parse-quil text))))
    (dolist (quil::*default-addresser-state-class*
             (list 'quil::temporal-addresser-state
                   'quil::fidelity-addresser-state))
      (dolist (quil::*addresser-use-1q-queues* (list t nil))
        (let* ((pp (quil::parse-quil text))
               (cpp (quil::compiler-hook pp chip :protoquil t)))
          (is (quil::operator= orig-mat (quil:parsed-program-to-logical-matrix cpp))))))))

(deftest test-unknown-instruction ()
  (let ((program (safely-parse-quil "a"))
        (chip (cl-quil::build-nq-fully-connected-chip 2)))
    (signals cl-quil:invalid-instruction-condition
      (cl-quil:compiler-hook program chip))))

(deftest test-fidelity-addresser-subschedule ()
  (flet ((gate= (gate-1 gate-2)
           (and (string= (quil::application-operator-root-name gate-1)
                         (quil::application-operator-root-name gate-2))
                (equalp (quil::application-parameters gate-1)
                        (quil::application-parameters gate-2))
                (equalp (quil::application-arguments gate-1)
                        (quil::application-arguments gate-2)))))

    (let* ((chip (quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (quil::chip-contiguous-subschedule-from-last-instructions
                    sched (quil::make-qubit-resource 0))
                   expected-subschedule))))

    (let* ((chip (quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (quil::make-chip-schedule chip))
             (expected-subschedule
               (list)))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (quil::chip-contiguous-subschedule-from-last-instructions
                    sched (quil::make-qubit-resource 1))
                   expected-subschedule))))
    
    (let* ((chip (quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "H" nil 1)
                     (build-gate "CNOT" nil 0 1)
                     (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (quil::chip-contiguous-subschedule-from-last-instructions
                    sched (quil::make-qubit-resource 0 1))
                   expected-subschedule))))

    (let* ((chip (quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; H 1; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "H" nil 1)
                     (build-gate "H" nil 1)
                     (build-gate "CNOT" nil 0 1)
                     (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (quil::chip-contiguous-subschedule-from-last-instructions
                    sched (quil::make-qubit-resource 0 1))
                   expected-subschedule))))

    (let* ((chip (quil::build-nq-linear-chip 3)))
      (let* ((progm (parse "H 0; H 1; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (quil::chip-contiguous-subschedule-from-last-instructions
                    sched (quil::make-qubit-resource 0 2))
                   expected-subschedule))))))

;;; Check that we can compile "parallel" programs onto disconnected
;;; chip components.
(deftest test-addresser-multiple-components ()
  (let ((program "DECLARE ro BIT[3]
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (not-signals error
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 3)))))

;;; Check that the naive assignment works correctly.
(deftest test-addresser-multiple-components-naive ()
  (let ((program "DECLARE ro BIT[3]
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (not-signals error
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 3)
                          :rewiring-type :naive))))

;;; Check that greedy assignments work as well.
(deftest test-addresser-multiple-components-greedy ()
  (let ((program "DECLARE ro BIT[3]
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (not-signals error
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 3)
                          :rewiring-type :greedy))))

;; Check that we can give a random rewiring type and the compiler will satisfy the component constraints.
(deftest test-addresser-multiple-components-explicit ()
  (let ((program "DECLARE ro BIT[3]
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (not-signals error
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 3)
                          :rewiring-type :random))))

;;; Check that we can compile a more complicated program on a more
;;; complicated chip, both with multiple connected components that
;;; cannot be matched with the identity function.
(deftest test-addresser-multiple-components-nontrivial ()
  (let ((program "DECLARE ro BIT[3]
CZ 0 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (not-signals error
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-chip-from-digraph '((1 2)))))))

;;; Check that we signal the right condition when the naive rewiring
;;; crosses chip component boundaries.
(deftest test-addresser-multiple-components-fails ()
  (let ((program "DECLARE ro BIT[3]
CZ 0 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (signals quil::naive-rewiring-crosses-chip-boundaries
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 3)
                          :rewiring-type :naive))))

;;; Check that we signal the right condition when there aren't enough qubits on the chip.
(deftest test-addresser-insufficient-qubits ()
  (let ((program "DECLARE ro BIT[3]
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (signals quil::chip-insufficient-qubits
      (quil:compiler-hook (quil:parse-quil program)
                          (quil::build-disconnected-chip 2)))))
