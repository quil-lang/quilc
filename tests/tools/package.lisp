;;;; tests/tools/package.lisp
;;;;
;;;; Author: Mark David

;;; Allegro (and other Lisps) don't support the non-standard "package
;;; local nicknames".
#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(fiasco:define-test-package #:cl-quil.tools-tests
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))
  (:use #:cl)

  ;; suite.lisp
  (:export
   #:run-tools-tests)
  (:shadowing-import-from #:cl-quil #:pi))
