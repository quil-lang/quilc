;;;; tests/package.lisp
;;;;
;;;; Author: Eric Peterson

(fiasco:define-test-package #:quilc-tests
  (:use #:quilc)
  
  ;; suite.lisp
  (:export
   #:run-quilc-tests)
  (:shadowing-import-from #:cl-quil #:pi))
