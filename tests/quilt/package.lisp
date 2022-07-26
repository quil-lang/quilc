;;;; tests/quilt/package.lisp
;;;;
;;;; Author: Erik Davis

(fiasco:define-test-package #:cl-quil.quilt-tests
  (:local-nicknames (#:a    #:alexandria)
                    (#:quil #:cl-quil))
  (:use #:cl-quil #:cl-quil.quilt)

  ;; suite.lisp
  (:export
   #:run-quilt-tests)
  (:shadowing-import-from #:cl-quil #:pi))
