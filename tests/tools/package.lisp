;;;; tests/tools/package.lisp
;;;;
;;;; Author: Mark David

(fiasco:define-test-package #:cl-quil/tools-tests
  (:local-nicknames (:a :alexandria))
  (:use #:cl)

  ;; suite.lisp
  (:export
   #:run-tools-tests)
  (:shadowing-import-from #:cl-quil #:pi))
