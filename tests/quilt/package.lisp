;;;; tests/quilt/package.lisp
;;;;
;;;; Author: Erik Davis

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil.quilt-tests
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:cl-quil #:cl-quil.quilt)

  ;; suite.lisp
  (:export
   #:run-quilt-tests)
  (:shadowing-import-from #:cl-quil #:pi))
