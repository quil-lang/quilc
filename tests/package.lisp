;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil-tests
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:cl-quil #:cl-quil.clifford)

  ;; suite.lisp
  (:export
   #:run-cl-quil-tests)
  (:shadowing-import-from #:cl-quil #:pi))
