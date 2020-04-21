;;;; addresser-tests.lisp
;;;;
;;;; Author: Eric Peterson

(in-package #:cl-quil-tests)

(deftest test-rewiring-shuffle ()
  (dolist (quil::*addresser-swap-search-type* '(:a* :greedy-qubit :greedy-path))
    (not-signals error
      (let ((text "CNOT 2 0"))
        (is (quil::operator=
             (quil::parsed-program-to-logical-matrix
              (quil::parse-quil text))
             (quil::parsed-program-to-logical-matrix
              (quil::compiler-hook (quil::parse-quil text) (quil::build-8q-chip)
                                   :protoquil nil :rewiring-type ':naive))))))))

(deftest test-addresser-subclasses ()
  (let* ((chip (quil::build-8q-chip))
         (text "CNOT 0 1 ; CNOT 0 2 ; CNOT 1 2")
         (orig-mat (quil::parsed-program-to-logical-matrix (quil::parse-quil text))))
    (dolist (quil::*default-addresser-state-class*
             (list 'quil::temporal-addresser-state
                   'quil::fidelity-addresser-state))
      (dolist (quil::*addresser-use-1q-queues* (list t nil))
        (let* ((pp (quil::parse-quil text))
               (cpp (quil::compiler-hook pp chip :protoquil t)))
          (is (quil::operator= orig-mat (quil::parsed-program-to-logical-matrix cpp))))))))

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
