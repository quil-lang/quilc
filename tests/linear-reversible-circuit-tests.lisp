;;;; linear-reversible-circuit.lisp
;;;;
;;;; Author: Charles Zhang

(in-package #:cl-quil-tests)

;;;; example matrices used in paper for testing

(defvar *fig-2-matrix*
  (magicl:from-list '(1 0 1 0
                      0 0 1 0
                      1 1 1 0
                      1 1 0 1)
                    '(4 4)))

(defvar *fig-3-matrix*
  (magicl:from-list '(1 1 0 0 0 0
                      1 0 0 1 1 0
                      0 1 0 0 1 0
                      1 1 1 1 1 1
                      1 1 0 1 1 1
                      0 0 1 1 1 0)
                    '(6 6)))

(defvar *swap-matrix*
  (magicl:from-list '(0 1
                      1 0)
                    '(2 2)))

(deftest test-linear-reversible-circuit-cnot-decomposition ()
  (macrolet ((test (square-matrix)
               `(is (magicl:= (magicl:deep-copy-tensor ,square-matrix)
                              (cl-quil::cnot-circuit-matrix (cl-quil::cnot-synth! ,square-matrix)
                                                         (first (magicl:shape ,square-matrix)))))))
    (test *fig-2-matrix*)
    (test *fig-3-matrix*)
    (test *swap-matrix*)))
