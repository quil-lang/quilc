;;;; tests/quilt/package.lisp
;;;;
;;;; Author: Erik Davis

(fiasco:define-test-package #:cl-quil/quilt-tests
  (:use #:cl-quil #:cl-quil/quilt)
  (:local-nicknames (#:quilt #:cl-quil/quilt))

  ;; suite.lisp
  (:export
   #:run-quilt-tests)
  (:shadowing-import-from #:cl-quil #:pi))
