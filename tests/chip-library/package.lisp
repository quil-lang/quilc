;;;; tests/chip-library/package.lisp
;;;;
;;;; Author: A.J. Nyquist

(fiasco:define-test-package #:cl-quil/chip-library-tests
  (:use #:cl #:cl-quil/chip-library)
  (:local-nicknames (:q :cl-quil))
  ;; suite.lisp
  (:export
   #:run-chip-library-tests))
