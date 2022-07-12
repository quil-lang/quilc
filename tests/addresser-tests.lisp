;;;; addresser-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(deftest test-rewiring-shuffle ()
  (dolist (cl-quil::*addresser-gates-swap-search-type* '(:a* :greedy-qubit :greedy-path))
    (not-signals error
      (let ((text "CNOT 2 0"))
        (is (cl-quil::operator=
             (cl-quil:parsed-program-to-logical-matrix
              (cl-quil::parse-quil text))
             (cl-quil:parsed-program-to-logical-matrix
              (cl-quil::compiler-hook (cl-quil::parse-quil text) (cl-quil::build-8q-chip)
                                   :protoquil nil :rewiring-type ':naive))))))))

(deftest test-addresser-subclasses ()
  (let* ((chip (cl-quil::build-8q-chip))
         (text "CNOT 0 1 ; CNOT 0 2 ; CNOT 1 2")
         (orig-mat (cl-quil:parsed-program-to-logical-matrix (cl-quil::parse-quil text))))
    (dolist (cl-quil::*default-addresser-state-class*
             (list 'cl-quil::temporal-addresser-state
                   'cl-quil::fidelity-addresser-state))
      (dolist (cl-quil::*addresser-use-1q-queues* (list t nil))
        (let* ((pp (cl-quil::parse-quil text))
               (cpp (cl-quil::compiler-hook pp chip :protoquil t)))
          (is (cl-quil::operator= orig-mat (cl-quil:parsed-program-to-logical-matrix cpp))))))))

(deftest test-fidelity-addresser-subschedule ()
  (flet ((gate= (gate-1 gate-2)
           (and (string= (cl-quil::application-operator-root-name gate-1)
                         (cl-quil::application-operator-root-name gate-2))
                (equalp (cl-quil::application-parameters gate-1)
                        (cl-quil::application-parameters gate-2))
                (equalp (cl-quil::application-arguments gate-1)
                        (cl-quil::application-arguments gate-2)))))

    (let* ((chip (cl-quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (cl-quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (cl-quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (cl-quil::chip-contiguous-subschedule-from-last-instructions
                    sched (cl-quil::make-qubit-resource 0))
                   expected-subschedule))))

    (let* ((chip (cl-quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (cl-quil::make-chip-schedule chip))
             (expected-subschedule
               (list)))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (cl-quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (cl-quil::chip-contiguous-subschedule-from-last-instructions
                    sched (cl-quil::make-qubit-resource 1))
                   expected-subschedule))))
    
    (let* ((chip (cl-quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (cl-quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "H" nil 1)
                     (build-gate "CNOT" nil 0 1)
                     (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (cl-quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (cl-quil::chip-contiguous-subschedule-from-last-instructions
                    sched (cl-quil::make-qubit-resource 0 1))
                   expected-subschedule))))

    (let* ((chip (cl-quil::build-nq-fully-connected-chip 3)))
      (let* ((progm (parse "H 0; H 1; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (cl-quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "H" nil 1)
                     (build-gate "H" nil 1)
                     (build-gate "CNOT" nil 0 1)
                     (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (cl-quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (cl-quil::chip-contiguous-subschedule-from-last-instructions
                    sched (cl-quil::make-qubit-resource 0 1))
                   expected-subschedule))))

    (let* ((chip (cl-quil::build-nq-linear-chip 3)))
      (let* ((progm (parse "H 0; H 1; CNOT 2 0; H 1; CNOT 0 1; X 0"))
             (sched (cl-quil::make-chip-schedule chip))
             (expected-subschedule
               (list (build-gate "X" nil 0))))
        (loop :for instr :across (parsed-program-executable-code progm) :do
          (cl-quil::chip-schedule-append sched instr))
        (is (every #'gate=
                   (cl-quil::chip-contiguous-subschedule-from-last-instructions
                    sched (cl-quil::make-qubit-resource 0 2))
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
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 3)))))

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
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 3)
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
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 3)
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
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 3)
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
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-chip-from-digraph '((1 2)))))))

;;; Check that we signal the right condition when the naive rewiring
;;; crosses chip component boundaries.
(deftest test-addresser-multiple-components-fails ()
  (let ((program "DECLARE ro BIT[3]
CZ 0 1
RX(pi/2) 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]"))
    (signals cl-quil::naive-rewiring-crosses-chip-boundaries
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 3)
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
    (signals cl-quil::chip-insufficient-qubits
      (cl-quil:compiler-hook (cl-quil:parse-quil program)
                          (cl-quil::build-disconnected-chip 2)))))
