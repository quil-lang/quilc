;;;; tests/package.lisp
;;;;
;;;; Author: Kartik Singh

(fiasco:define-test-package #:libquilc-tests
    (:use #:quilc)

  ;; suite.lisp
  (:export
   #:run-libquilc-tests)
  (:shadowing-import-from #:cl-quil))
