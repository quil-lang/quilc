;;;; tests/package.lisp
;;;;
;;;; Author: Eric Peterson

(fiasco:define-test-package #:quilc-tests
  (:use #:quilc)
  
  ;; suite.lisp
  (:export
   #:run-quilc-tests)
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil))
  (:shadowing-import-from #:cl-quil #:pi))
