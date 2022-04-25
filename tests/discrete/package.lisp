;;;; tests/discrete/package.lisp
;;;;
;;;; Author: Robert Smith

(fiasco:define-test-package #:cl-quil.discrete-tests
  (:use #:cl)
  ;; suite.lisp
  (:export
   #:run-discrete-tests))
