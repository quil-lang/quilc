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
      (let* ((pp (quil::parse-quil text))
             (cpp (quil::compiler-hook pp chip :protoquil t)))
        (is (quil::operator= orig-mat (quil::parsed-program-to-logical-matrix cpp)))))))
