;;;; linear-reversible-circuit.lisp
;;;;
;;;; Author: Charles Zhang

(in-package #:cl-quil-tests)

(a:define-constant +prime+ #(0 2 3 5 7 1 4 6) :test #'equalp)

;; Test that internal assertions are all satisfied.
(deftest permutation-internal-assertion ()
  (quil::synthesize-permutation #(0 1))
  (quil::synthesize-permutation #(1 0))
  (quil::synthesize-permutation #(3 0 1 2))
  (quil::synthesize-permutation +prime+))
