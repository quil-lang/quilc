;;;; src/discrete/parse-tests.lisp
;;;;
;;;; Author: A.J. Nyquist

(in-package #:cl-quil.discrete-tests)

(deftest parse-tolerance ()
  (cl-quil:parse-quil "PRAGMA TOLERANCE \"0.1234\"")
  (is (<= (- cl-quil.discrete::*tolerance* 0.1234) 1d-16))
  (cl-quil:parse-quil "PRAGMA TOLERANCE \"2.0e-2\"")
  (is (<= (- cl-quil.discrete::*tolerance* 2.0d-2) 1d-16))
  (signals type-error
    (cl-quil:parse-quil "PRAGMA TOLERANCE \"0\""))
  (signals type-error
    (cl-quil:parse-quil "PRAGMA TOLERANCE \"1\"")))

(deftest compile-rz ()
  ;; While nothing is done with this result,
  ;; It at the very least assures it runs without errors.
  (q::compiler-hook
   (q:parse-quil "PRAGMA TOLERANCE \"1E-10\"; RZ(pi/3) 0")
   (cl-quil.discrete:build-discrete-linear-chip
    1 :compile-fast t)))
