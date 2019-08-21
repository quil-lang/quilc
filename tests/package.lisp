;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

#+(or allegro)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil-tests
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:cl-quil #:cl-quil.clifford)

  ;; suite.lisp
  (:export
   #:run-cl-quil-tests)
  (:shadowing-import-from #:cl-quil #:pi))
