;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

(fiasco:define-test-package #:cl-quil-tests
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil))
  (:use #:cl-quil #:cl-quil/clifford)

  ;; suite.lisp
  (:export
   #:run-cl-quil-tests)
  (:shadowing-import-from #:cl-quil #:pi))
