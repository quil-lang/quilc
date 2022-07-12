;;;; tests/chip-library/chip-table-tests.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil/chip-library-tests)

(deftest default-chips ()
  (is (equalp (call-chip-builder "8q") (q::build-8q-chip)))
  (is (equalp (call-chip-builder "20Q") (q::build-skew-rectangular-chip 0 4 5))))

(deftest define-chip ()
  ;; Uses the non-sense name ""
  (install-chip-builder "" nil :no-warn t)
  (is (not (install-chip-builder "" #'q::build-skew-rectangular-chip)))
  (is (equalp (q::build-skew-rectangular-chip 0 1 2)
              (call-chip-builder "" 0 1 2)))
  (is (install-chip-builder "" nil :no-warn t))
  (is (not (install-chip-builder "" nil))))
