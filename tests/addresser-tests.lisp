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
