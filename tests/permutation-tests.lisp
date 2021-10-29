;;;; permutation-tests.lisp
;;;;
;;;; Author: Charles Zhang

(in-package #:cl-quil-tests)

(a:define-constant +prime+ #(0 2 3 5 7 1 4 6) :test #'equalp)

(defun matrix-from-permutation (permutation)
  (let* ((size (length permutation))
         (matrix (magicl:zeros (list size size) :type '(complex double-float))))
    (loop :for i :from 0
          :for j :across permutation
          :do (setf (magicl:tref matrix j i) 1))
    matrix))

(defun permutation-synthesis-as-parsed-program (permutation)
  (make-instance 'quil::parsed-program
                 :executable-code (coerce (quil::synthesize-permutation permutation) 'vector)))

;;; Test that the synthesized permutation when simulated performs the
;;; action of the permutation.
(deftest test-permutation-gates-logical-matrix-equivalent ()
  (flet ((test (permutation)
           (is (quil::operator=
                (parsed-program-to-logical-matrix
                 (permutation-synthesis-as-parsed-program permutation)
                 :compress-qubits nil)
                (matrix-from-permutation permutation)))))
    (test #(0 1))
    (test #(1 0))
    (test #(3 0 1 2))
    (test #(2 0 3 1))
    (test #(1 0 3 2))
    (test #(0 2 3 1))
    (test #(2 1 3 0))
    (test #(3 1 2 0))
    (test +prime+)))
